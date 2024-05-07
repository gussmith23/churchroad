/*
 *  yosys -- Yosys Open SYnthesis Suite
 *
 *  Copyright (C) 2012  Claire Xenia Wolf <claire@yosyshq.com>
 *
 *  Permission to use, copy, modify, and/or distribute this software for any
 *  purpose with or without fee is hereby granted, provided that the above
 *  copyright notice and this permission notice appear in all copies.
 *
 *  THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 *  WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 *  MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 *  ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 *  WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 *  ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 *  OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *
 */

#include "kernel/celltypes.h"
#include "kernel/json.h"
#include "kernel/log.h"
#include "kernel/mem.h"
#include "kernel/register.h"
#include "kernel/rtlil.h"
#include "kernel/sigtools.h"
#include "kernel/yw.h"
#include <string>
#include <cassert>

USING_YOSYS_NAMESPACE
PRIVATE_NAMESPACE_BEGIN

struct LakeroadWorker
{
	std::ostream &f;
	SigMap sigmap;
	RTLIL::Module *module;
	bool verbose;
	bool single_bad;
	bool cover_mode;
	bool print_internal_names;

	int next_nid = 1;
	int initstate_nid = -1;

	// <width> => <sid>
	dict<int, int> sorts_bv;

	// (<address-width>, <data-width>) => <sid>
	dict<pair<int, int>, int> sorts_mem;

	// SigBit => (<nid>, <bitidx>)
	dict<SigBit, pair<int, int>> bit_nid;

	// <nid> => <bvwidth>
	dict<int, int> nid_width;

	// SigSpec => <nid>
	dict<SigSpec, int> sig_nid;

	// bit to driving cell
	dict<SigBit, Cell *> bit_cell;

	// nids for constants
	dict<Const, int> consts;

	// ff inputs that need to be evaluated (<nid>, <ff_cell>)
	vector<pair<int, Cell *>> ff_todo;
	vector<pair<int, Mem *>> mem_todo;

	pool<Cell *> cell_recursion_guard;
	vector<int> bad_properties;
	dict<SigBit, bool> initbits;
	pool<Wire *> statewires;
	pool<string> srcsymbols;
	vector<Mem> memories;
	dict<Cell *, Mem *> mem_cells;

	string indent, info_filename;
	vector<string> info_lines;
	dict<int, int> info_clocks;

	struct ywmap_btor_sig
	{
		SigSpec sig;
		Cell *cell = nullptr;

		ywmap_btor_sig(const SigSpec &sig) : sig(sig) {}
		ywmap_btor_sig(Cell *cell) : cell(cell) {}
	};

	vector<ywmap_btor_sig> ywmap_inputs;
	vector<ywmap_btor_sig> ywmap_states;
	dict<SigBit, int> ywmap_clock_bits;
	dict<SigBit, int> ywmap_clock_inputs;

	PrettyJson ywmap_json;

	void btorf(const char *fmt, ...) YS_ATTRIBUTE(format(printf, 2, 3))
	{
		va_list ap;
		va_start(ap, fmt);
		f << indent << vstringf(fmt, ap);
		va_end(ap);
	}

	void infof(const char *fmt, ...) YS_ATTRIBUTE(format(printf, 2, 3))
	{
		va_list ap;
		va_start(ap, fmt);
		info_lines.push_back(vstringf(fmt, ap));
		va_end(ap);
	}

	template <typename T>
	string getinfo(T *obj, bool srcsym = false)
	{
		string infostr = log_id(obj);
		if (!srcsym && !print_internal_names && infostr[0] == '$')
			return "";
		if (obj->attributes.count(ID::src))
		{
			string src = obj->attributes.at(ID::src).decode_string().c_str();
			if (srcsym && infostr[0] == '$')
			{
				std::replace(src.begin(), src.end(), ' ', '_');
				if (srcsymbols.count(src) || module->count_id("\\" + src))
				{
					for (int i = 1;; i++)
					{
						string s = stringf("%s-%d", src.c_str(), i);
						if (!srcsymbols.count(s) && !module->count_id("\\" + s))
						{
							src = s;
							break;
						}
					}
				}
				srcsymbols.insert(src);
				infostr = src;
			}
			else
			{
				infostr += " ; " + src;
			}
		}
		return " " + infostr;
	}

	void ywmap_state(const SigSpec &sig)
	{
		if (ywmap_json.active())
			ywmap_states.emplace_back(sig);
	}

	void ywmap_state(Cell *cell)
	{
		if (ywmap_json.active())
			ywmap_states.emplace_back(cell);
	}

	void ywmap_input(const SigSpec &sig)
	{
		if (ywmap_json.active())
			ywmap_inputs.emplace_back(sig);
	}

	void emit_ywmap_btor_sig(const ywmap_btor_sig &btor_sig)
	{
		if (btor_sig.cell == nullptr)
		{
			if (btor_sig.sig.empty())
			{
				ywmap_json.value(nullptr);
				return;
			}
			ywmap_json.begin_array();
			ywmap_json.compact();
			for (auto &chunk : btor_sig.sig.chunks())
			{
				log_assert(chunk.is_wire());

				ywmap_json.begin_object();
				ywmap_json.entry("path", witness_path(chunk.wire));
				ywmap_json.entry("width", chunk.width);
				ywmap_json.entry("offset", chunk.offset);
				ywmap_json.end_object();
			}
			ywmap_json.end_array();
		}
		else
		{
			ywmap_json.begin_object();
			ywmap_json.compact();
			ywmap_json.entry("path", witness_path(btor_sig.cell));
			Mem *mem = mem_cells[btor_sig.cell];
			ywmap_json.entry("width", mem->width);
			ywmap_json.entry("size", mem->size);
			ywmap_json.end_object();
		}
	}

	void btorf_push(const string &id)
	{
		if (verbose)
		{
			f << indent << stringf("  ; begin %s\n", id.c_str());
			indent += "    ";
		}
	}

	void btorf_pop(const string &id)
	{
		if (verbose)
		{
			indent = indent.substr(4);
			f << indent << stringf("  ; end %s\n", id.c_str());
		}
	}

	int get_bv_sid(int width)
	{
		if (sorts_bv.count(width) == 0)
		{
			int nid = next_nid++;
			btorf("%d sort bitvec %d\n", nid, width);
			sorts_bv[width] = nid;
		}
		return sorts_bv.at(width);
	}

	int get_mem_sid(int abits, int dbits)
	{
		pair<int, int> key(abits, dbits);
		if (sorts_mem.count(key) == 0)
		{
			int addr_sid = get_bv_sid(abits);
			int data_sid = get_bv_sid(dbits);
			int nid = next_nid++;
			btorf("%d sort array %d %d\n", nid, addr_sid, data_sid);
			sorts_mem[key] = nid;
		}
		return sorts_mem.at(key);
	}

	void add_nid_sig(int nid, const SigSpec &sig)
	{
		if (verbose)
			f << indent << stringf("; %d %s\n", nid, log_signal(sig));

		for (int i = 0; i < GetSize(sig); i++)
			bit_nid[sig[i]] = make_pair(nid, i);

		sig_nid[sig] = nid;
		nid_width[nid] = GetSize(sig);
	}

	void export_cell(Cell *cell)
	{
		if (cell_recursion_guard.count(cell))
		{
			string cell_list;
			for (auto c : cell_recursion_guard)
				cell_list += stringf("\n    %s", log_id(c));
			log_error("Found topological loop while processing cell %s. Active cells:%s\n", log_id(cell), cell_list.c_str());
		}

		cell_recursion_guard.insert(cell);
		btorf_push(log_id(cell));

		if (cell->type.in(ID($add), ID($sub), ID($mul), ID($and), ID($or), ID($xor), ID($xnor), ID($shl), ID($sshl), ID($shr), ID($sshr),
											ID($shift), ID($shiftx), ID($concat), ID($_AND_), ID($_NAND_), ID($_OR_), ID($_NOR_), ID($_XOR_), ID($_XNOR_)))
		{
			string btor_op;
			if (cell->type == ID($add))
				btor_op = "add";
			if (cell->type == ID($sub))
				btor_op = "sub";
			if (cell->type == ID($mul))
				btor_op = "mul";
			if (cell->type.in(ID($shl), ID($sshl)))
				btor_op = "sll";
			if (cell->type == ID($shr))
				btor_op = "srl";
			if (cell->type == ID($sshr))
				btor_op = "sra";
			if (cell->type.in(ID($shift), ID($shiftx)))
				btor_op = "shift";
			if (cell->type.in(ID($and), ID($_AND_)))
				btor_op = "and";
			if (cell->type.in(ID($or), ID($_OR_)))
				btor_op = "or";
			if (cell->type.in(ID($xor), ID($_XOR_)))
				btor_op = "xor";
			if (cell->type == ID($concat))
				btor_op = "concat";
			if (cell->type == ID($_NAND_))
				btor_op = "nand";
			if (cell->type == ID($_NOR_))
				btor_op = "nor";
			if (cell->type.in(ID($xnor), ID($_XNOR_)))
				btor_op = "xnor";
			log_assert(!btor_op.empty());

			int width_ay = std::max(GetSize(cell->getPort(ID::A)), GetSize(cell->getPort(ID::Y)));
			int width = std::max(width_ay, GetSize(cell->getPort(ID::B)));

			bool a_signed = cell->hasParam(ID::A_SIGNED) ? cell->getParam(ID::A_SIGNED).as_bool() : false;
			bool b_signed = cell->hasParam(ID::B_SIGNED) ? cell->getParam(ID::B_SIGNED).as_bool() : false;

			if (btor_op == "shift" && !b_signed)
				btor_op = "srl";

			if (cell->type.in(ID($shl), ID($sshl), ID($shr), ID($sshr)))
				b_signed = false;

			if (cell->type == ID($sshr) && !a_signed)
				btor_op = "srl";

			int sid = get_bv_sid(width);
			int nid;

			int nid_a;
			if (cell->type.in(ID($shl), ID($shr), ID($shift), ID($shiftx)) && a_signed && width_ay < width)
			{
				// sign-extend A up to the width of Y
				int nid_a_padded = get_sig_nid(cell->getPort(ID::A), width_ay, a_signed);

				// zero-extend the rest
				int zeroes = get_sig_nid(Const(0, width - width_ay));
				nid_a = next_nid++;
				btorf("%d concat %d %d %d\n", nid_a, sid, zeroes, nid_a_padded);
			}
			else
			{
				nid_a = get_sig_nid(cell->getPort(ID::A), width, a_signed);
			}

			int nid_b = get_sig_nid(cell->getPort(ID::B), width, b_signed);

			if (btor_op == "shift")
			{
				int nid_r = next_nid++;
				btorf("%d srl %d %d %d\n", nid_r, sid, nid_a, nid_b);

				int nid_b_neg = next_nid++;
				btorf("%d neg %d %d\n", nid_b_neg, sid, nid_b);

				int nid_l = next_nid++;
				btorf("%d sll %d %d %d\n", nid_l, sid, nid_a, nid_b_neg);

				int sid_bit = get_bv_sid(1);
				int nid_zero = get_sig_nid(Const(0, width));
				int nid_b_ltz = next_nid++;
				btorf("%d slt %d %d %d\n", nid_b_ltz, sid_bit, nid_b, nid_zero);

				nid = next_nid++;
				btorf("%d ite %d %d %d %d%s\n", nid, sid, nid_b_ltz, nid_l, nid_r, getinfo(cell).c_str());
			}
			else
			{
				nid = next_nid++;
				btorf("%d %s %d %d %d%s\n", nid, btor_op.c_str(), sid, nid_a, nid_b, getinfo(cell).c_str());
			}

			SigSpec sig = sigmap(cell->getPort(ID::Y));

			if (GetSize(sig) < width)
			{
				int sid = get_bv_sid(GetSize(sig));
				int nid2 = next_nid++;
				btorf("%d slice %d %d %d 0\n", nid2, sid, nid, GetSize(sig) - 1);
				nid = nid2;
			}

			add_nid_sig(nid, sig);
			goto okay;
		}

		if (cell->type.in(ID($div), ID($mod), ID($modfloor)))
		{
			bool a_signed = cell->hasParam(ID::A_SIGNED) ? cell->getParam(ID::A_SIGNED).as_bool() : false;
			bool b_signed = cell->hasParam(ID::B_SIGNED) ? cell->getParam(ID::B_SIGNED).as_bool() : false;

			string btor_op;
			if (cell->type == ID($div))
				btor_op = "div";
			// "rem" = truncating modulo
			if (cell->type == ID($mod))
				btor_op = "rem";
			// "mod" = flooring modulo
			if (cell->type == ID($modfloor))
			{
				// "umod" doesn't exist because it's the same as "urem"
				btor_op = a_signed || b_signed ? "mod" : "rem";
			}
			log_assert(!btor_op.empty());

			int width = GetSize(cell->getPort(ID::Y));
			width = std::max(width, GetSize(cell->getPort(ID::A)));
			width = std::max(width, GetSize(cell->getPort(ID::B)));

			int nid_a = get_sig_nid(cell->getPort(ID::A), width, a_signed);
			int nid_b = get_sig_nid(cell->getPort(ID::B), width, b_signed);

			int sid = get_bv_sid(width);
			int nid = next_nid++;
			btorf("%d %c%s %d %d %d%s\n", nid, a_signed || b_signed ? 's' : 'u', btor_op.c_str(), sid, nid_a, nid_b,
						getinfo(cell).c_str());

			SigSpec sig = sigmap(cell->getPort(ID::Y));

			if (GetSize(sig) < width)
			{
				int sid = get_bv_sid(GetSize(sig));
				int nid2 = next_nid++;
				btorf("%d slice %d %d %d 0\n", nid2, sid, nid, GetSize(sig) - 1);
				nid = nid2;
			}

			add_nid_sig(nid, sig);
			goto okay;
		}

		if (cell->type.in(ID($_ANDNOT_), ID($_ORNOT_)))
		{
			int sid = get_bv_sid(1);
			int nid_a = get_sig_nid(cell->getPort(ID::A));
			int nid_b = get_sig_nid(cell->getPort(ID::B));

			int nid1 = next_nid++;
			int nid2 = next_nid++;

			if (cell->type == ID($_ANDNOT_))
			{
				btorf("%d not %d %d\n", nid1, sid, nid_b);
				btorf("%d and %d %d %d%s\n", nid2, sid, nid_a, nid1, getinfo(cell).c_str());
			}

			if (cell->type == ID($_ORNOT_))
			{
				btorf("%d not %d %d\n", nid1, sid, nid_b);
				btorf("%d or %d %d %d%s\n", nid2, sid, nid_a, nid1, getinfo(cell).c_str());
			}

			SigSpec sig = sigmap(cell->getPort(ID::Y));
			add_nid_sig(nid2, sig);
			goto okay;
		}

		if (cell->type.in(ID($_OAI3_), ID($_AOI3_)))
		{
			int sid = get_bv_sid(1);
			int nid_a = get_sig_nid(cell->getPort(ID::A));
			int nid_b = get_sig_nid(cell->getPort(ID::B));
			int nid_c = get_sig_nid(cell->getPort(ID::C));

			int nid1 = next_nid++;
			int nid2 = next_nid++;
			int nid3 = next_nid++;

			if (cell->type == ID($_OAI3_))
			{
				btorf("%d or %d %d %d\n", nid1, sid, nid_a, nid_b);
				btorf("%d and %d %d %d\n", nid2, sid, nid1, nid_c);
				btorf("%d not %d %d%s\n", nid3, sid, nid2, getinfo(cell).c_str());
			}

			if (cell->type == ID($_AOI3_))
			{
				btorf("%d and %d %d %d\n", nid1, sid, nid_a, nid_b);
				btorf("%d or %d %d %d\n", nid2, sid, nid1, nid_c);
				btorf("%d not %d %d%s\n", nid3, sid, nid2, getinfo(cell).c_str());
			}

			SigSpec sig = sigmap(cell->getPort(ID::Y));
			add_nid_sig(nid3, sig);
			goto okay;
		}

		if (cell->type.in(ID($_OAI4_), ID($_AOI4_)))
		{
			int sid = get_bv_sid(1);
			int nid_a = get_sig_nid(cell->getPort(ID::A));
			int nid_b = get_sig_nid(cell->getPort(ID::B));
			int nid_c = get_sig_nid(cell->getPort(ID::C));
			int nid_d = get_sig_nid(cell->getPort(ID::D));

			int nid1 = next_nid++;
			int nid2 = next_nid++;
			int nid3 = next_nid++;
			int nid4 = next_nid++;

			if (cell->type == ID($_OAI4_))
			{
				btorf("%d or %d %d %d\n", nid1, sid, nid_a, nid_b);
				btorf("%d or %d %d %d\n", nid2, sid, nid_c, nid_d);
				btorf("%d and %d %d %d\n", nid3, sid, nid1, nid2);
				btorf("%d not %d %d%s\n", nid4, sid, nid3, getinfo(cell).c_str());
			}

			if (cell->type == ID($_AOI4_))
			{
				btorf("%d and %d %d %d\n", nid1, sid, nid_a, nid_b);
				btorf("%d and %d %d %d\n", nid2, sid, nid_c, nid_d);
				btorf("%d or %d %d %d\n", nid3, sid, nid1, nid2);
				btorf("%d not %d %d%s\n", nid4, sid, nid3, getinfo(cell).c_str());
			}

			SigSpec sig = sigmap(cell->getPort(ID::Y));
			add_nid_sig(nid4, sig);
			goto okay;
		}

		if (cell->type.in(ID($lt), ID($le), ID($eq), ID($eqx), ID($ne), ID($nex), ID($ge), ID($gt)))
		{
			string btor_op;
			if (cell->type == ID($lt))
				btor_op = "lt";
			if (cell->type == ID($le))
				btor_op = "lte";
			if (cell->type.in(ID($eq), ID($eqx)))
				btor_op = "eq";
			if (cell->type.in(ID($ne), ID($nex)))
				btor_op = "neq";
			if (cell->type == ID($ge))
				btor_op = "gte";
			if (cell->type == ID($gt))
				btor_op = "gt";
			log_assert(!btor_op.empty());

			int width = 1;
			width = std::max(width, GetSize(cell->getPort(ID::A)));
			width = std::max(width, GetSize(cell->getPort(ID::B)));

			bool a_signed = cell->hasParam(ID::A_SIGNED) ? cell->getParam(ID::A_SIGNED).as_bool() : false;
			bool b_signed = cell->hasParam(ID::B_SIGNED) ? cell->getParam(ID::B_SIGNED).as_bool() : false;

			int sid = get_bv_sid(1);
			int nid_a = get_sig_nid(cell->getPort(ID::A), width, a_signed);
			int nid_b = get_sig_nid(cell->getPort(ID::B), width, b_signed);

			int nid = next_nid++;
			if (cell->type.in(ID($lt), ID($le), ID($ge), ID($gt)))
			{
				btorf("%d %c%s %d %d %d%s\n", nid, a_signed || b_signed ? 's' : 'u', btor_op.c_str(), sid, nid_a, nid_b,
							getinfo(cell).c_str());
			}
			else
			{
				btorf("%d %s %d %d %d%s\n", nid, btor_op.c_str(), sid, nid_a, nid_b, getinfo(cell).c_str());
			}

			SigSpec sig = sigmap(cell->getPort(ID::Y));

			if (GetSize(sig) > 1)
			{
				int sid = get_bv_sid(GetSize(sig));
				int nid2 = next_nid++;
				btorf("%d uext %d %d %d\n", nid2, sid, nid, GetSize(sig) - 1);
				nid = nid2;
			}

			add_nid_sig(nid, sig);
			goto okay;
		}

		if (cell->type.in(ID($not), ID($neg), ID($_NOT_), ID($pos)))
		{
			string btor_op;
			if (cell->type.in(ID($not), ID($_NOT_)))
				btor_op = "not";
			if (cell->type == ID($neg))
				btor_op = "neg";

			int width = std::max(GetSize(cell->getPort(ID::A)), GetSize(cell->getPort(ID::Y)));

			bool a_signed = cell->hasParam(ID::A_SIGNED) ? cell->getParam(ID::A_SIGNED).as_bool() : false;
			int nid_a = get_sig_nid(cell->getPort(ID::A), width, a_signed);
			SigSpec sig = sigmap(cell->getPort(ID::Y));

			// the $pos cell just passes through, all other cells need an actual operation applied
			int nid = nid_a;
			if (cell->type != ID($pos))
			{
				log_assert(!btor_op.empty());
				int sid = get_bv_sid(width);
				nid = next_nid++;
				btorf("%d %s %d %d%s\n", nid, btor_op.c_str(), sid, nid_a, getinfo(cell).c_str());
			}

			if (GetSize(sig) < width)
			{
				int sid = get_bv_sid(GetSize(sig));
				int nid2 = next_nid++;
				btorf("%d slice %d %d %d 0\n", nid2, sid, nid, GetSize(sig) - 1);
				nid = nid2;
			}

			add_nid_sig(nid, sig);
			goto okay;
		}

		if (cell->type.in(ID($logic_and), ID($logic_or), ID($logic_not)))
		{
			string btor_op;
			if (cell->type == ID($logic_and))
				btor_op = "and";
			if (cell->type == ID($logic_or))
				btor_op = "or";
			if (cell->type == ID($logic_not))
				btor_op = "not";
			log_assert(!btor_op.empty());

			int sid = get_bv_sid(1);
			int nid_a = get_sig_nid(cell->getPort(ID::A));
			int nid_b = btor_op != "not" ? get_sig_nid(cell->getPort(ID::B)) : 0;

			if (GetSize(cell->getPort(ID::A)) > 1)
			{
				int nid_red_a = next_nid++;
				btorf("%d redor %d %d\n", nid_red_a, sid, nid_a);
				nid_a = nid_red_a;
			}

			if (btor_op != "not" && GetSize(cell->getPort(ID::B)) > 1)
			{
				int nid_red_b = next_nid++;
				btorf("%d redor %d %d\n", nid_red_b, sid, nid_b);
				nid_b = nid_red_b;
			}

			int nid = next_nid++;
			if (btor_op != "not")
				btorf("%d %s %d %d %d%s\n", nid, btor_op.c_str(), sid, nid_a, nid_b, getinfo(cell).c_str());
			else
				btorf("%d %s %d %d%s\n", nid, btor_op.c_str(), sid, nid_a, getinfo(cell).c_str());

			SigSpec sig = sigmap(cell->getPort(ID::Y));

			if (GetSize(sig) > 1)
			{
				int sid = get_bv_sid(GetSize(sig));
				int zeros_nid = get_sig_nid(Const(0, GetSize(sig) - 1));
				int nid2 = next_nid++;
				btorf("%d concat %d %d %d\n", nid2, sid, zeros_nid, nid);
				nid = nid2;
			}

			add_nid_sig(nid, sig);
			goto okay;
		}

		if (cell->type.in(ID($reduce_and), ID($reduce_or), ID($reduce_bool), ID($reduce_xor), ID($reduce_xnor)))
		{
			string btor_op;
			if (cell->type == ID($reduce_and))
				btor_op = "redand";
			if (cell->type.in(ID($reduce_or), ID($reduce_bool)))
				btor_op = "redor";
			if (cell->type.in(ID($reduce_xor), ID($reduce_xnor)))
				btor_op = "redxor";
			log_assert(!btor_op.empty());

			int sid = get_bv_sid(1);
			int nid_a = get_sig_nid(cell->getPort(ID::A));

			int nid = next_nid++;

			if (cell->type == ID($reduce_xnor))
			{
				int nid2 = next_nid++;
				btorf("%d %s %d %d%s\n", nid, btor_op.c_str(), sid, nid_a, getinfo(cell).c_str());
				btorf("%d not %d %d\n", nid2, sid, nid);
				nid = nid2;
			}
			else
			{
				btorf("%d %s %d %d%s\n", nid, btor_op.c_str(), sid, nid_a, getinfo(cell).c_str());
			}

			SigSpec sig = sigmap(cell->getPort(ID::Y));

			if (GetSize(sig) > 1)
			{
				int sid = get_bv_sid(GetSize(sig));
				int zeros_nid = get_sig_nid(Const(0, GetSize(sig) - 1));
				int nid2 = next_nid++;
				btorf("%d concat %d %d %d\n", nid2, sid, zeros_nid, nid);
				nid = nid2;
			}

			add_nid_sig(nid, sig);
			goto okay;
		}

		if (cell->type.in(ID($mux), ID($_MUX_), ID($_NMUX_)))
		{
			SigSpec sig_a = sigmap(cell->getPort(ID::A));
			SigSpec sig_b = sigmap(cell->getPort(ID::B));
			SigSpec sig_s = sigmap(cell->getPort(ID::S));
			SigSpec sig_y = sigmap(cell->getPort(ID::Y));

			int nid_a = get_sig_nid(sig_a);
			int nid_b = get_sig_nid(sig_b);
			int nid_s = get_sig_nid(sig_s);

			int sid = get_bv_sid(GetSize(sig_y));
			int nid = next_nid++;

			if (cell->type == ID($_NMUX_))
			{
				int tmp = nid;
				nid = next_nid++;
				btorf("%d ite %d %d %d %d\n", tmp, sid, nid_s, nid_b, nid_a);
				btorf("%d not %d %d%s\n", nid, sid, tmp, getinfo(cell).c_str());
			}
			else
			{
				btorf("%d ite %d %d %d %d%s\n", nid, sid, nid_s, nid_b, nid_a, getinfo(cell).c_str());
			}

			add_nid_sig(nid, sig_y);
			goto okay;
		}

		if (cell->type == ID($pmux))
		{
			SigSpec sig_a = sigmap(cell->getPort(ID::A));
			SigSpec sig_b = sigmap(cell->getPort(ID::B));
			SigSpec sig_s = sigmap(cell->getPort(ID::S));
			SigSpec sig_y = sigmap(cell->getPort(ID::Y));

			int width = GetSize(sig_a);
			int sid = get_bv_sid(width);
			int nid = get_sig_nid(sig_a);

			for (int i = 0; i < GetSize(sig_s); i++)
			{
				int nid_b = get_sig_nid(sig_b.extract(i * width, width));
				int nid_s = get_sig_nid(sig_s.extract(i));
				int nid2 = next_nid++;
				if (i == GetSize(sig_s) - 1)
					btorf("%d ite %d %d %d %d%s\n", nid2, sid, nid_s, nid_b, nid, getinfo(cell).c_str());
				else
					btorf("%d ite %d %d %d %d\n", nid2, sid, nid_s, nid_b, nid);
				nid = nid2;
			}

			add_nid_sig(nid, sig_y);
			goto okay;
		}

		if (cell->type.in(ID($dff), ID($ff), ID($anyinit), ID($_DFF_P_), ID($_DFF_N), ID($_FF_)))
		{
			SigSpec sig_d = sigmap(cell->getPort(ID::D));
			SigSpec sig_q = sigmap(cell->getPort(ID::Q));

			if ((!info_filename.empty() || ywmap_json.active()) && cell->type.in(ID($dff), ID($_DFF_P_), ID($_DFF_N_)))
			{
				SigSpec sig_c = sigmap(cell->getPort(cell->type == ID($dff) ? ID::CLK : ID::C));
				int nid = get_sig_nid(sig_c);
				bool negedge = false;

				if (cell->type == ID($_DFF_N_))
					negedge = true;

				if (cell->type == ID($dff) && !cell->getParam(ID::CLK_POLARITY).as_bool())
					negedge = true;

				if (!info_filename.empty())
					info_clocks[nid] |= negedge ? 2 : 1;

				if (ywmap_json.active())
					ywmap_clock_bits[sig_c] |= negedge ? 2 : 1;
			}

			IdString symbol;

			if (sig_q.is_wire())
			{
				Wire *w = sig_q.as_wire();
				if (w->port_id == 0)
				{
					statewires.insert(w);
					symbol = w->name;
				}
			}

			Const initval;
			for (int i = 0; i < GetSize(sig_q); i++)
				if (initbits.count(sig_q[i]))
					initval.bits.push_back(initbits.at(sig_q[i]) ? State::S1 : State::S0);
				else
					initval.bits.push_back(State::Sx);

			int nid_init_val = -1;

			if (!initval.is_fully_undef())
				nid_init_val = get_sig_nid(initval, -1, false, true);

			int sid = get_bv_sid(GetSize(sig_q));
			int nid = next_nid++;

			if (symbol.empty() || (!print_internal_names && symbol[0] == '$'))
				btorf("%d state %d\n", nid, sid);
			else
				btorf("%d state %d %s\n", nid, sid, log_id(symbol));

			if (cell->get_bool_attribute(ID(clk2fflogic)))
				ywmap_state(cell->getPort(ID::D)); // For a clk2fflogic FF the named signal is the D input not the Q output
			else
				ywmap_state(sig_q);

			if (nid_init_val >= 0)
			{
				int nid_init = next_nid++;
				if (verbose)
					btorf("; initval = %s\n", log_signal(initval));
				btorf("%d init %d %d %d\n", nid_init, sid, nid, nid_init_val);
			}

			ff_todo.push_back(make_pair(nid, cell));
			add_nid_sig(nid, sig_q);
			goto okay;
		}

		if (cell->type.in(ID($anyconst), ID($anyseq)))
		{
			SigSpec sig_y = sigmap(cell->getPort(ID::Y));

			int sid = get_bv_sid(GetSize(sig_y));
			int nid = next_nid++;

			btorf("%d state %d%s\n", nid, sid, getinfo(cell).c_str());

			ywmap_state(sig_y);

			if (cell->type == ID($anyconst))
			{
				int nid2 = next_nid++;
				btorf("%d next %d %d %d\n", nid2, sid, nid, nid);
			}

			add_nid_sig(nid, sig_y);
			goto okay;
		}

		if (cell->type == ID($initstate))
		{
			SigSpec sig_y = sigmap(cell->getPort(ID::Y));

			if (initstate_nid < 0)
			{
				int sid = get_bv_sid(1);
				int one_nid = get_sig_nid(State::S1);
				int zero_nid = get_sig_nid(State::S0);
				initstate_nid = next_nid++;
				btorf("%d state %d%s\n", initstate_nid, sid, getinfo(cell).c_str());
				btorf("%d init %d %d %d\n", next_nid++, sid, initstate_nid, one_nid);
				btorf("%d next %d %d %d\n", next_nid++, sid, initstate_nid, zero_nid);

				ywmap_state(sig_y);
			}

			add_nid_sig(initstate_nid, sig_y);
			goto okay;
		}

		if (cell->is_mem_cell())
		{
			Mem *mem = mem_cells[cell];

			int abits = ceil_log2(mem->size);

			bool asyncwr = false;
			bool syncwr = false;

			for (auto &port : mem->wr_ports)
			{
				if (port.clk_enable)
					syncwr = true;
				else
					asyncwr = true;
			}

			if (asyncwr && syncwr)
				log_error("Memory %s.%s has mixed async/sync write ports.\n", log_id(module), log_id(mem->memid));

			for (auto &port : mem->rd_ports)
			{
				if (port.clk_enable)
					log_error("Memory %s.%s has sync read ports.  Please use memory_nordff to convert them first.\n",
										log_id(module), log_id(mem->memid));
			}

			int data_sid = get_bv_sid(mem->width);
			int bool_sid = get_bv_sid(1);
			int sid = get_mem_sid(abits, mem->width);

			int nid_init_val = -1;

			if (!mem->inits.empty())
			{
				Const initdata = mem->get_init_data();
				bool constword = true;
				Const firstword = initdata.extract(0, mem->width);

				for (int i = 1; i < mem->size; i++)
				{
					Const thisword = initdata.extract(i * mem->width, mem->width);
					if (thisword != firstword)
					{
						constword = false;
						break;
					}
				}

				if (constword)
				{
					if (verbose)
						btorf("; initval = %s\n", log_signal(firstword));
					nid_init_val = get_sig_nid(firstword, -1, false, true);
				}
				else
				{
					nid_init_val = next_nid++;
					btorf("%d state %d\n", nid_init_val, sid);

					ywmap_state(nullptr);

					for (int i = 0; i < mem->size; i++)
					{
						Const thisword = initdata.extract(i * mem->width, mem->width);
						if (thisword.is_fully_undef())
							continue;
						Const thisaddr(i, abits);
						int nid_thisword = get_sig_nid(thisword, -1, false, true);
						int nid_thisaddr = get_sig_nid(thisaddr, -1, false, true);
						int last_nid_init_val = nid_init_val;
						nid_init_val = next_nid++;
						if (verbose)
							btorf("; initval[%d] = %s\n", i, log_signal(thisword));
						btorf("%d write %d %d %d %d\n", nid_init_val, sid, last_nid_init_val, nid_thisaddr, nid_thisword);
					}
				}
			}

			int nid = next_nid++;
			int nid_head = nid;

			if (mem->memid[0] == '$')
				btorf("%d state %d\n", nid, sid);
			else
				btorf("%d state %d %s\n", nid, sid, log_id(mem->memid));

			ywmap_state(cell);

			if (nid_init_val >= 0)
			{
				int nid_init = next_nid++;
				btorf("%d init %d %d %d\n", nid_init, sid, nid, nid_init_val);
			}

			if (asyncwr)
			{
				for (auto &port : mem->wr_ports)
				{
					SigSpec wa = port.addr;
					wa.extend_u0(abits);

					int wa_nid = get_sig_nid(wa);
					int wd_nid = get_sig_nid(port.data);
					int we_nid = get_sig_nid(port.en);

					int nid2 = next_nid++;
					btorf("%d read %d %d %d\n", nid2, data_sid, nid_head, wa_nid);

					int nid3 = next_nid++;
					btorf("%d not %d %d\n", nid3, data_sid, we_nid);

					int nid4 = next_nid++;
					btorf("%d and %d %d %d\n", nid4, data_sid, nid2, nid3);

					int nid5 = next_nid++;
					btorf("%d and %d %d %d\n", nid5, data_sid, wd_nid, we_nid);

					int nid6 = next_nid++;
					btorf("%d or %d %d %d\n", nid6, data_sid, nid5, nid4);

					int nid7 = next_nid++;
					btorf("%d write %d %d %d %d\n", nid7, sid, nid_head, wa_nid, nid6);

					int nid8 = next_nid++;
					btorf("%d redor %d %d\n", nid8, bool_sid, we_nid);

					int nid9 = next_nid++;
					btorf("%d ite %d %d %d %d\n", nid9, sid, nid8, nid7, nid_head);

					nid_head = nid9;
				}
			}

			for (auto &port : mem->rd_ports)
			{
				SigSpec ra = port.addr;
				ra.extend_u0(abits);

				int ra_nid = get_sig_nid(ra);
				int rd_nid = next_nid++;

				btorf("%d read %d %d %d\n", rd_nid, data_sid, nid_head, ra_nid);

				add_nid_sig(rd_nid, port.data);
			}

			if (!asyncwr)
			{
				mem_todo.push_back(make_pair(nid, mem));
			}
			else
			{
				int nid2 = next_nid++;
				btorf("%d next %d %d %d\n", nid2, sid, nid, nid_head);
			}

			goto okay;
		}

		if (cell->type.in(ID($dffe), ID($sdff), ID($sdffe), ID($sdffce)) || cell->type.str().substr(0, 6) == "$_SDFF" ||
				(cell->type.str().substr(0, 6) == "$_DFFE" && cell->type.str().size() == 10))
		{
			log_error("Unsupported cell type %s for cell %s.%s -- please run `dffunmap` before `write_btor`.\n", log_id(cell->type),
								log_id(module), log_id(cell));
		}
		if (cell->type.in(ID($adff), ID($adffe), ID($aldff), ID($aldffe), ID($dffsr), ID($dffsre)) ||
				cell->type.str().substr(0, 5) == "$_DFF" || cell->type.str().substr(0, 7) == "$_ALDFF")
		{
			log_error(
					"Unsupported cell type %s for cell %s.%s -- please run `async2sync; dffunmap` or `clk2fflogic` before `write_btor`.\n",
					log_id(cell->type), log_id(module), log_id(cell));
		}
		if (cell->type.in(ID($sr), ID($dlatch), ID($adlatch), ID($dlatchsr)) || cell->type.str().substr(0, 8) == "$_DLATCH" ||
				cell->type.str().substr(0, 5) == "$_SR_")
		{
			log_error("Unsupported cell type %s for cell %s.%s -- please run `clk2fflogic` before `write_btor`.\n", log_id(cell->type),
								log_id(module), log_id(cell));
		}
		log_error("Unsupported cell type %s for cell %s.%s.\n", log_id(cell->type), log_id(module), log_id(cell));

	okay:
		btorf_pop(log_id(cell));
		cell_recursion_guard.erase(cell);
	}

	int get_sig_nid(SigSpec sig, int to_width = -1, bool is_signed = false, bool is_init = false)
	{
		int nid = -1;
		sigmap.apply(sig);

		for (auto bit : sig)
			if (bit == State::Sx)
				goto has_undef_bits;

		if (0)
		{
		has_undef_bits:
			SigSpec sig_mask_undef, sig_noundef;
			int first_undef = -1;

			for (int i = 0; i < GetSize(sig); i++)
				if (sig[i] == State::Sx)
				{
					if (first_undef < 0)
						first_undef = i;
					sig_mask_undef.append(State::S1);
					sig_noundef.append(State::S0);
				}
				else
				{
					sig_mask_undef.append(State::S0);
					sig_noundef.append(sig[i]);
				}

			if (to_width < 0 || first_undef < to_width)
			{
				int sid = get_bv_sid(GetSize(sig));

				int nid_input = next_nid++;
				if (is_init)
				{
					btorf("%d state %d\n", nid_input, sid);
					ywmap_state(sig);
				}
				else
				{
					btorf("%d input %d\n", nid_input, sid);
					ywmap_input(sig);
				}

				int nid_masked_input;
				if (sig_mask_undef.is_fully_ones())
				{
					nid_masked_input = nid_input;
				}
				else
				{
					int nid_mask_undef = get_sig_nid(sig_mask_undef);
					nid_masked_input = next_nid++;
					btorf("%d and %d %d %d\n", nid_masked_input, sid, nid_input, nid_mask_undef);
				}

				if (sig_noundef.is_fully_zero())
				{
					nid = nid_masked_input;
				}
				else
				{
					int nid_noundef = get_sig_nid(sig_noundef);
					nid = next_nid++;
					btorf("%d or %d %d %d\n", nid, sid, nid_masked_input, nid_noundef);
				}

				goto extend_or_trim;
			}

			sig = sig_noundef;
		}

		if (sig_nid.count(sig) == 0)
		{
			// <nid>, <bitidx>
			vector<pair<int, int>> nidbits;

			// collect all bits
			for (int i = 0; i < GetSize(sig); i++)
			{
				SigBit bit = sig[i];

				if (bit_nid.count(bit) == 0)
				{
					if (bit.wire == nullptr)
					{
						Const c(bit.data);

						while (i + GetSize(c) < GetSize(sig) && sig[i + GetSize(c)].wire == nullptr)
							c.bits.push_back(sig[i + GetSize(c)].data);

						if (consts.count(c) == 0)
						{
							int sid = get_bv_sid(GetSize(c));
							int nid = next_nid++;
							btorf("%d const %d %s\n", nid, sid, c.as_string().c_str());
							consts[c] = nid;
							nid_width[nid] = GetSize(c);
						}

						int nid = consts.at(c);

						for (int j = 0; j < GetSize(c); j++)
							nidbits.push_back(make_pair(nid, j));

						i += GetSize(c) - 1;
						continue;
					}
					else
					{
						if (bit_cell.count(bit) == 0)
						{
							SigSpec s = bit;

							while (i + GetSize(s) < GetSize(sig) && sig[i + GetSize(s)].wire != nullptr &&
										 bit_cell.count(sig[i + GetSize(s)]) == 0)
								s.append(sig[i + GetSize(s)]);

							log_warning("No driver for signal %s.\n", log_signal(s));

							int sid = get_bv_sid(GetSize(s));
							int nid = next_nid++;
							btorf("%d input %d\n", nid, sid);
							ywmap_input(s);
							nid_width[nid] = GetSize(s);

							for (int j = 0; j < GetSize(s); j++)
								nidbits.push_back(make_pair(nid, j));

							i += GetSize(s) - 1;
							continue;
						}
						else
						{
							export_cell(bit_cell.at(bit));
							log_assert(bit_nid.count(bit));
						}
					}
				}

				nidbits.push_back(bit_nid.at(bit));
			}

			int width = 0;
			int nid = -1;

			// group bits and emit slice-concat chain
			for (int i = 0; i < GetSize(nidbits); i++)
			{
				int nid2 = nidbits[i].first;
				int lower = nidbits[i].second;
				int upper = lower;

				while (i + 1 < GetSize(nidbits) && nidbits[i + 1].first == nidbits[i].first &&
							 nidbits[i + 1].second == nidbits[i].second + 1)
					upper++, i++;

				int nid3 = nid2;

				if (lower != 0 || upper + 1 != nid_width.at(nid2))
				{
					int sid = get_bv_sid(upper - lower + 1);
					nid3 = next_nid++;
					btorf("%d slice %d %d %d %d\n", nid3, sid, nid2, upper, lower);
				}

				int nid4 = nid3;

				if (nid >= 0)
				{
					int sid = get_bv_sid(width + upper - lower + 1);
					nid4 = next_nid++;
					btorf("%d concat %d %d %d\n", nid4, sid, nid3, nid);
				}

				width += upper - lower + 1;
				nid = nid4;
			}

			sig_nid[sig] = nid;
			nid_width[nid] = width;
		}

		nid = sig_nid.at(sig);

	extend_or_trim:
		if (to_width >= 0 && to_width != GetSize(sig))
		{
			if (to_width < GetSize(sig))
			{
				int sid = get_bv_sid(to_width);
				int nid2 = next_nid++;
				btorf("%d slice %d %d %d 0\n", nid2, sid, nid, to_width - 1);
				nid = nid2;
			}
			else
			{
				int sid = get_bv_sid(to_width);
				int nid2 = next_nid++;
				btorf("%d %s %d %d %d\n", nid2, is_signed ? "sext" : "uext", sid, nid, to_width - GetSize(sig));
				nid = nid2;
			}
		}

		return nid;
	}

	LakeroadWorker(std::ostream &f, RTLIL::Module *module) : f(f), sigmap(module), module(module) {}

	void run()
	{
		if (module->processes.size() > 0)
			log_error("Module has processes.\n");

		// IDs used to generate let expressions.
		int id = 0;
		auto get_new_id_str = [&]()
		{ return stringf("v%d", id++); };

		// Get signal name.
		auto get_signal_name = [&](const SigSpec &sig)
		{
			auto signal_name = std::string(log_signal(sig));
			assert(signal_name.size() > 0);
			if (signal_name[0] == '\\')
				signal_name = signal_name.substr(1);
			return signal_name;
		};

		// Function to generate a let expression.
		auto let = [&](const std::string &id_str, const std::string &expr)
		{ return stringf("(let %s %s)", id_str.c_str(), expr.c_str()); };

		// Wire expressions (which we can eventually delete)
		vector<std::string> wire_exprs;

		// Generate wire expression.
		auto wire_expr = [&](const std::string &name, const int bitwidth)
		{
			auto s = stringf("(Wire \"%s\" %d)", name.c_str(), bitwidth);
			wire_exprs.push_back(s);
			return s;
		};

		std::map<RTLIL::SigSpec, std::string> signal_let_bound_name;

		// Note: we `sigmap` all wires here, so there's no need to `sigmap` them
		// recursively within get_expression_for_signal. This was a source of an
		// infinite loop: https://github.com/uwsampl/yosys/issues/10
		//
		// Does not sigmap the signal; you should sigmap the signal if you need it
		// sigmapped.
		//
		// to_width = -1 means no extension.
		std::function<std::string(const SigSpec &, int)> get_expression_for_signal = [&](const SigSpec &sig, int to_width)
		{
			std::function<std::string(std::string, const SigSpec &, int)> convert_to_width = [&](std::string expr, const SigSpec &sig, int to_width)
			{
				auto out_expr = expr;

				// If we need to extend the signal, do so.
				if (to_width >= 0 && to_width != GetSize(sig))
				{
					if (to_width < GetSize(sig))
					{
						auto new_id = get_new_id_str();
						auto extend_expr = stringf("(Op1 (Extract %d %d) %s)", to_width - 1, 0, expr.c_str());
						f << let(new_id, extend_expr) << "\n";
						out_expr = new_id;
					}
					else
					{

						auto new_id = get_new_id_str();
						f << "; TODO not handling signedness\n";
						auto extend_expr = stringf("(Op1 (ZeroExtend %d) %s)", to_width, expr.c_str());
						f << let(new_id, extend_expr) << "\n";
						out_expr = new_id;
					}
				}

				return out_expr;
			};

			// If we've already handled the expression, return it.
			if (signal_let_bound_name.count(sig))
				return convert_to_width(signal_let_bound_name.at(sig), sig, to_width);

			// SigSpecs are either constants, wires, or concatenations and selections
			// of wires. We simply need to handle each case.

			// The output expression will go in here.
			std::string out_expr;

			if (sig.is_fully_const())
			{
				// If the signal is a constant, we can just use the constant.
				auto const_str = stringf("(Op0 (BV %d %d))", Const(sig.as_const()).as_int(), sig.size());
				auto new_id = get_new_id_str();
				auto let_expr = let(new_id, const_str);
				auto signal_name = get_signal_name(sig);
				f << "; " << signal_name << "\n";
				f << let_expr << "\n";
				out_expr = new_id;
			}
			else if (sig.is_wire())
			{
				// If the signal is a simple wire, we can just use the name of the wire.
				auto signal_name = get_signal_name(sig);
				auto let_bound_id = get_new_id_str();
				// Generate wire expression
				auto let_expr = let(let_bound_id, wire_expr(let_bound_id, GetSize(sig)));
				f << "; " << signal_name << "\n";
				f << let_expr << "\n";
				out_expr = let_bound_id;
			}
			else if (sig.chunks().size() > 1)
			{
				// If the signal is multiple chunks, we need to generate a concatenation.

				f << "; " << log_signal(sig) << "\n";

				// Generate expression for each chunk.
				std::vector<std::string> chunk_exprs;
				for (auto chunk : sig.chunks())
				{
					chunk_exprs.push_back(get_expression_for_signal(chunk, -1));
				}

				// Generate concatenation expressions.
				auto concat_expr = chunk_exprs[0];
				for (size_t i = 1; i < chunk_exprs.size(); i++)
				{
					auto new_id = get_new_id_str();
					auto let_expr = let(new_id, stringf("(Op2 (Concat) %s %s)", concat_expr.c_str(), chunk_exprs[i].c_str()));
					f << let_expr << "\n";
					concat_expr = new_id;
				}

				out_expr = concat_expr;
			}
			else if (sig.chunks().size() == 1 && sig.chunks()[0].wire->width != sig.size())
			{
				// This branch is meant to capture the case where the signal is a
				// slice/extraction. I'm not quite sure how to check this in Yosys
				// though.
				//
				// It may be the case that we've set up these if statements in a way
				// that this is the only possible case that's left. That would be nice.
				// Currently, the condition in this else if branch is a little messy.

				auto chunk = sig.chunks()[0];

				if (chunk.wire->upto)
				{
					// You can copy implementation for upto from verilog_backend.cc.
					log_error("Unimplemented case of `upto==true` in %s.\n", log_signal(sig));
				}

				// The let-bound ID string of the expression to extract from.
				auto extract_from_expr = get_expression_for_signal(sig.chunks()[0].wire, -1);
				auto new_id = get_new_id_str();
				auto extract_expr = stringf("(Op1 (Extract %d %d) %s)", (chunk.offset + chunk.width - 1) + chunk.wire->start_offset,
																		chunk.offset + chunk.wire->start_offset, extract_from_expr.c_str());

				auto let_expr = let(new_id, extract_expr);
				f << let_expr << "\n";
				out_expr = new_id;
			}
			else
			{
				log_error("Unhandled case of signal for %s.\n", log_signal(sig));
			}

			signal_let_bound_name.insert({sig, out_expr});
			return convert_to_width(out_expr, sig, to_width);
		};

		// Create Wire expression for each wire.
		// We don't have to do this upfront anymore. Expressions can just be created
		// as needed.
		f << "; wire declarations\n";
		for (auto wire : module->wires())
		{
			get_expression_for_signal(sigmap(wire), -1);
		}

		// Handle cells
		f << "\n; cells\n";
		for (auto cell : module->cells())
		{

			if (cell->type.in(ID($logic_not), ID($not), ID($reduce_or), ID($reduce_bool), ID($reduce_and), ID($reduce_xor)))
			{
				// Unary ops.
				assert(cell->connections().size() == 2);
				auto y = sigmap(cell->getPort(ID::Y));
				auto a_let_name = get_expression_for_signal(sigmap(cell->getPort(ID::A)), y.size());
				auto y_let_name = get_expression_for_signal(y, -1);

				std::string op_str;
				if (cell->type == ID($logic_not))
					op_str = "(LogicNot)";
				else if (cell->type == ID($not))
					op_str = "(Not)";
				else if (cell->type.in(ID($reduce_or), ID($reduce_bool)))
					op_str = "(ReduceOr)";
				else if (cell->type == ID($reduce_and))
					op_str = "(ReduceAnd)";
				else if (cell->type == ID($reduce_xor))
					op_str = "(ReduceXor)";
				else
					log_error("This should be unreachable. You are missing an else if branch.\n");

				f << stringf("(union %s (Op1 %s %s))\n", y_let_name.c_str(), op_str.c_str(), a_let_name.c_str()).c_str();
			}
			else if (cell->type.in(ID($slice)))
			{
				// Slice.
				assert(cell->connections().size() == 2);
				// Do we need sigmap here?
				auto y = sigmap(cell->getPort(ID::Y));
				auto a_let_name = get_expression_for_signal(sigmap(cell->getPort(ID::A)), -1);
				auto y_let_name = get_expression_for_signal(y, -1);

				// Get OFFSET and Y_WIDTH parameters.
				auto offset = cell->getParam(ID::OFFSET).as_int();
				auto y_width = cell->getParam(ID::Y_WIDTH).as_int();

				std::string op_str;
				if (cell->type == ID($slice))
					op_str = stringf("(Extract %d %d)", offset + y_width - 1, offset).c_str();
				else
					log_error("This should be unreachable. You are missing an else if branch.\n");

				f << stringf("(union %s (Op1 %s %s))\n", y_let_name.c_str(), op_str.c_str(), a_let_name.c_str()).c_str();
			}
			else if (cell->type.in(ID($and), ID($or), ID($xor), ID($shr), ID($add), ID($shiftx), ID($mul), ID($sub)))
			{
				// Assert that A and B are both unsigned. Note that this is a
				// simplifying assumption. It does not have to be true, but supporting
				// different signedness would require some thought that I'm not putting
				// in right now.
				assert(cell->getParam(ID::A_SIGNED).is_fully_zero());
				assert(cell->getParam(ID::B_SIGNED).is_fully_zero());

				// Get the max width of the inputs. This determines the width we need to
				// extend both inputs to.
				auto max_input_width = std::max(cell->getPort(ID::A).size(), cell->getPort(ID::B).size());

				assert(cell->connections().size() == 3);
				auto a_let_name = get_expression_for_signal(cell->getPort(ID::A), max_input_width);
				auto b_let_name = get_expression_for_signal(cell->getPort(ID::B), max_input_width);
				auto y_let_name = get_expression_for_signal(cell->getPort(ID::Y), -1);

				std::string op_str;
				if (cell->type == ID($and))
					op_str = "(And)";
				else if (cell->type == ID($or))
					op_str = "(Or)";
				else if (cell->type == ID($xor))
					op_str = "(Xor)";
				// Here, $shr and $shiftx are treated the same.
				// This is only true because we've asserted that A and B are unsigned.
				// See #26:
				// https://github.com/uwsampl/churchroad/issues/26
				else if (cell->type.in(ID($shr), ID($shiftx)))
					op_str = "(Shr)";
				else if (cell->type == ID($add))
					op_str = "(Add)";
				else if (cell->type == ID($mul))
					op_str = "(Mul)";
				else if (cell->type == ID($sub))
					op_str = "(Sub)";
				else
					log_error("This should be unreachable. You are missing an else if branch.\n");

				op_str = stringf("(Op2 %s %s %s)", op_str.c_str(), a_let_name.c_str(),
												 b_let_name.c_str());

				// If the output width is less than the result of the operation, we need
				// to slice the result.
				//
				// This is an assumption we're currently making. Doesn't have to be the
				// case. We may also need to extend the result in the future, when this
				// assertion fails.
				assert(cell->getPort(ID::Y).size() <= max_input_width);
				if (cell->getPort(ID::Y).size() < max_input_width)
					op_str = stringf("(Op1 (Extract %d %d) %s)", cell->getPort(ID::Y).size() - 1, 0, op_str.c_str());

				f << stringf("(union %s %s)\n", y_let_name.c_str(), op_str.c_str()).c_str();
			}
			else if (cell->type.in(ID($concat)))
			{
				// Concat.
				assert(cell->connections().size() == 3);
				// do we need this sigmap?
				auto y = sigmap(cell->getPort(ID::Y));
				auto a_let_name = get_expression_for_signal(sigmap(cell->getPort(ID::A)), -1);
				auto b_let_name = get_expression_for_signal(sigmap(cell->getPort(ID::B)), -1);
				auto y_let_name = get_expression_for_signal(y, -1);

				std::string op_str;
				if (cell->type == ID($concat))
					op_str = "(Concat)";
				else
					log_error("This should be unreachable. You are missing an else if branch.\n");

				// Here, we assume that the semantics of Yosys' $concat is Y = {B, A}.
				// https://www.reddit.com/r/yosys/comments/4e9co6/very_basic_question_about_cellmapping/
				f << stringf("(union %s (Op2 %s %s %s))\n", y_let_name.c_str(), op_str.c_str(), b_let_name.c_str(),
										 a_let_name.c_str())
								 .c_str();
			}
			else if (cell->type.in(ID($mux)))
			{
				assert(cell->connections().size() == 4);
				auto y = sigmap(cell->getPort(ID::Y));
				// TODO(@gussmith23): Should we cast to y.size() here?
				// If these fail, it's probably because splice/splitnets wasn't run.
				assert(cell->getPort(ID::A).size() == y.size());
				assert(cell->getPort(ID::B).size() == y.size());
				auto a_let_name = get_expression_for_signal(sigmap(cell->getPort(ID::A)), y.size());
				auto b_let_name = get_expression_for_signal(sigmap(cell->getPort(ID::B)), y.size());
				auto s_let_name = get_expression_for_signal(sigmap(cell->getPort(ID::S)), -1);
				auto y_let_name = get_expression_for_signal(y, -1);

				f << stringf("(union %s (Op3 (Mux) %s %s %s))\n", y_let_name.c_str(), s_let_name.c_str(), a_let_name.c_str(),
										 b_let_name.c_str())
								 .c_str();
			}
			else if (cell->type.in(ID($eq), ID($logic_and), ID($logic_or), ID($ne)))
			{
				// Binary ops that result in one bit.
				assert(cell->connections().size() == 3);
				auto y = sigmap(cell->getPort(ID::Y));
				auto a = sigmap(cell->getPort(ID::A));
				auto b = sigmap(cell->getPort(ID::B));

				if (y.size() != 1)
					log_error("Expected 1-bit output for cell %s.\n", log_id(cell));

				// Extend the inputs to the same width.
				// TODO(@gussmith23): there might be Yosys pass to do this
				// automatically; see:
				// https://github.com/YosysHQ/yosys/discussions/4368
				int to_width = std::max(a.size(), b.size());
				auto a_let_name = get_expression_for_signal(sigmap(cell->getPort(ID::A)), to_width);
				auto b_let_name = get_expression_for_signal(sigmap(cell->getPort(ID::B)), to_width);

				auto y_let_name = get_expression_for_signal(y, -1);

				std::string op_str;
				if (cell->type == ID($eq))
					op_str = "(Eq)";
				else if (cell->type == ID($logic_and))
					op_str = "(LogicAnd)";
				else if (cell->type == ID($logic_or))
					op_str = "(LogicOr)";
				else if (cell->type == ID($ne))
					op_str = "(Ne)";
				else
					log_error("This should be unreachable. You are missing an else if branch.\n");

				f << stringf("(union %s (Op2 %s %s %s))\n", y_let_name.c_str(), op_str.c_str(), a_let_name.c_str(),
										 b_let_name.c_str())
								 .c_str();
			}
			else if (cell->type == ID($dff))
			{
				assert(cell->connections().size() == 3);
				auto q = sigmap(cell->getPort(ID::Q));
				auto d_let_name = get_expression_for_signal(sigmap(cell->getPort(ID::D)), q.size());
				auto clk_let_name = get_expression_for_signal(sigmap(cell->getPort(ID::CLK)), -1);
				auto q_let_name = get_expression_for_signal(q, -1);

				f << "; TODO: assuming 0 default for Reg\n";
				f << stringf("(union %s (Op2 (Reg 0) %s %s))\n", q_let_name.c_str(), clk_let_name.c_str(), d_let_name.c_str());
			}
			else if (cell->type == ID($pmux))
			{
				// Don't support $pmux: require them to run pmuxtree instead.
				log_error("Unsupported cell type %s for cell %s.%s -- please run `pmuxtree` before `write_lakeroad`.\n",
									log_id(cell->type), log_id(module), log_id(cell));
			}
			else if (cell->has_attribute("\\src"))
			{
				// Instance of a user-defined module.
				// TODO(@gussmith23): is this the best way to determine whether it's a
				// user-defined module? By this I mean, not an inbuilt primitive to
				// Yosys.

				std::vector<std::pair<std::string, std::string>> input_port_names_and_exprs;
				std::vector<std::pair<std::string, std::string>> output_port_names_and_exprs;

				for (auto connection : cell->connections())
				{
					auto port = connection.first;
					auto port_name = port.str();
					auto sig = connection.second;
					auto sig_let_name = get_expression_for_signal(sig, -1);

					assert(cell->input(port) || cell->output(port));

					if (cell->input(port))
					{
						input_port_names_and_exprs.push_back({port_name, sig_let_name});
					}
					else if (cell->output(port))
					{
						output_port_names_and_exprs.push_back({port_name, sig_let_name});
					}
				}

				// Generate the instance.
				// Cut the "\" off the front.
				// Check that it starts with "\" first, though.
				assert(cell->type[0] == '\\');
				assert(cell->name[0] == '\\');
				f << stringf("(let %s (ModuleInstance \"%s\" (vec-of", cell->name.substr(1).c_str(), cell->type.substr(1).c_str()).c_str();
				for (auto [port_name, _] : input_port_names_and_exprs)
				{
					assert(port_name[0] == '\\');
					f << stringf(" \"%s\"", port_name.substr(1).c_str()).c_str();
				}
				f << ") (vec-of";
				for (auto [_, expr] : input_port_names_and_exprs)
				{
					f << stringf(" %s", expr.c_str()).c_str();
				}
				f << ")))\n";

				// Hook up the outputs.
				for (auto [port_name, expr] : output_port_names_and_exprs)
				{
					assert(port_name[0] == '\\');
					assert(cell->name[0] == '\\');
					f << stringf("(union (GetOutput %s \"%s\") %s)\n", cell->name.substr(1).c_str(), port_name.substr(1).c_str(), expr.c_str()).c_str();
				}
			}
			else
			{
				log_error("Unimplemented cell type %s for cell %s.%s.\n", log_id(cell->type), log_id(module), log_id(cell));
			}
		}

		// For each input, generate Var expression and mark it as an input port
		// using the IsPort relation. Also, union it with the corresponding wire.
		f << "\n; inputs\n";
		for (auto wire : module->wires())
		{
			if (!wire->port_id || !wire->port_input)
				continue;

			auto sigspec = sigmap(wire);
			auto signal_name = get_signal_name(sigspec);

			assert(signal_let_bound_name.count(sigspec));
			auto let_bound_id = signal_let_bound_name.at(sigspec);

			f << stringf("(let %s (Var \"%s\" %d))\n", signal_name.c_str(), signal_name.c_str(), GetSize(sigspec)).c_str();
			f << stringf("(IsPort \"%s\" \"%s\" (Input) %s)\n", /*module name*/ "", signal_name.c_str(), signal_name.c_str()).c_str();
			f << stringf("(union %s %s)\n", let_bound_id.c_str(), signal_name.c_str()).c_str();
		}

		// For each output, mark it as an output port using the IsPort relation.
		f << "\n; outputs\n";
		for (auto wire : module->wires())
		{
			if (!wire->port_id || !wire->port_output)
				continue;

			// The name before sigmapping should be the output port name, i hope!
			auto signal_name_pre_sigmap = get_signal_name(wire);

			// The name after sigmapping will help us figure out which let ID holds the value of this signal.
			auto sigspec = sigmap(wire);
			auto signal_name = get_signal_name(sigspec);
			assert(signal_let_bound_name.count(sigspec));
			auto let_bound_id = signal_let_bound_name.at(sigspec);

			f << stringf("(let %s %s)\n", signal_name_pre_sigmap.c_str(), let_bound_id.c_str()).c_str();
			f << stringf("(IsPort \"%s\" \"%s\" (Output) %s)\n", /*module name*/ "", signal_name_pre_sigmap.c_str(), signal_name_pre_sigmap.c_str()).c_str();
		}

		// Delete Wire expressions.
		f << "\n; delete wire expressions\n";
		for (auto wire_expr : wire_exprs)
		{
			f << stringf("(delete %s)\n", wire_expr.c_str()).c_str();
		}

		return;
		for (auto wire : module->wires())
		{
			// Some kind of initialization thing?
			// if (wire->attributes.count(ID::init)) {
			// 	Const attrval = wire->attributes.at(ID::init);
			// 	for (int i = 0; i < GetSize(wire) && i < GetSize(attrval); i++)
			// 		if (attrval[i] == State::S0 || attrval[i] == State::S1)
			// 			initbits[sigmap(SigBit(wire, i))] = (attrval[i] == State::S1);
			// }

			// Skip if not an input?
			if (!wire->port_id || !wire->port_input)
				continue;

			SigSpec sig = sigmap(wire);
			auto signal_name = std::string(log_signal(sig));
			assert(signal_name.size() > 1);
			signal_name = signal_name.substr(1);
			auto id_str = "v0";
			btorf("(let %s (Var %s %d))\n", id_str, signal_name.c_str(), GetSize(sig));

			// if (!info_filename.empty()) {
			// 	auto gclk_attr = wire->attributes.find(ID::replaced_by_gclk);
			// 	if (gclk_attr != wire->attributes.end()) {
			// 		if (gclk_attr->second == State::S1)
			// 			info_clocks[nid] |= 1;
			// 		else if (gclk_attr->second == State::S0)
			// 			info_clocks[nid] |= 2;
			// 	}
			// }

			// if (ywmap_json.active()) {
			// 	for (int i = 0; i < GetSize(sig); i++) {
			// 		auto input_bit = SigBit(wire, i);
			// 		auto bit = sigmap(input_bit);
			// 		if (!ywmap_clock_bits.count(bit))
			// 			continue;
			// 		ywmap_clock_inputs[input_bit] = ywmap_clock_bits[bit];
			// 	}
			// }
		}

		// btorf_pop("inputs");

		// for (auto cell : module->cells())
		// 	for (auto &conn : cell->connections()) {
		// 		if (!cell->output(conn.first))
		// 			continue;

		// 		for (auto bit : sigmap(conn.second))
		// 			bit_cell[bit] = cell;
		// 	}

		// for (auto wire : module->wires()) {
		// 	if (!wire->port_id || !wire->port_output)
		// 		continue;

		// 	btorf_push(stringf("output %s", log_id(wire)));

		// 	int nid = get_sig_nid(wire);
		// 	btorf("%d output %d%s\n", next_nid++, nid, getinfo(wire).c_str());

		// 	btorf_pop(stringf("output %s", log_id(wire)));
		// }

		// for (auto wire : module->wires()) {
		// 	if (wire->port_id || wire->name[0] == '$')
		// 		continue;

		// 	btorf_push(stringf("wire %s", log_id(wire)));

		// 	int sid = get_bv_sid(GetSize(wire));
		// 	int nid = get_sig_nid(sigmap(wire));

		// 	if (statewires.count(wire))
		// 		continue;

		// 	int this_nid = next_nid++;
		// 	btorf("%d uext %d %d %d%s\n", this_nid, sid, nid, 0, getinfo(wire).c_str());
		// 	if (info_clocks.count(nid))
		// 		info_clocks[this_nid] |= info_clocks[nid];

		// 	btorf_pop(stringf("wire %s", log_id(wire)));
		// 	continue;
		// }

		// while (!ff_todo.empty() || !mem_todo.empty()) {
		// 	vector<pair<int, Cell *>> todo;
		// 	todo.swap(ff_todo);

		// 	for (auto &it : todo) {
		// 		int nid = it.first;
		// 		Cell *cell = it.second;

		// 		btorf_push(stringf("next %s", log_id(cell)));

		// 		SigSpec sig = sigmap(cell->getPort(ID::D));
		// 		int nid_q = get_sig_nid(sig);
		// 		int sid = get_bv_sid(GetSize(sig));
		// 		btorf("%d next %d %d %d%s\n", next_nid++, sid, nid, nid_q, getinfo(cell).c_str());

		// 		btorf_pop(stringf("next %s", log_id(cell)));
		// 	}

		// 	vector<pair<int, Mem *>> mtodo;
		// 	mtodo.swap(mem_todo);

		// 	for (auto &it : mtodo) {
		// 		int nid = it.first;
		// 		Mem *mem = it.second;

		// 		btorf_push(stringf("next %s", log_id(mem->memid)));

		// 		int abits = ceil_log2(mem->size);

		// 		int data_sid = get_bv_sid(mem->width);
		// 		int bool_sid = get_bv_sid(1);
		// 		int sid = get_mem_sid(abits, mem->width);
		// 		int nid_head = nid;

		// 		for (auto &port : mem->wr_ports) {
		// 			SigSpec wa = port.addr;
		// 			wa.extend_u0(abits);

		// 			int wa_nid = get_sig_nid(wa);
		// 			int wd_nid = get_sig_nid(port.data);
		// 			int we_nid = get_sig_nid(port.en);

		// 			int nid2 = next_nid++;
		// 			btorf("%d read %d %d %d\n", nid2, data_sid, nid_head, wa_nid);

		// 			int nid3 = next_nid++;
		// 			btorf("%d not %d %d\n", nid3, data_sid, we_nid);

		// 			int nid4 = next_nid++;
		// 			btorf("%d and %d %d %d\n", nid4, data_sid, nid2, nid3);

		// 			int nid5 = next_nid++;
		// 			btorf("%d and %d %d %d\n", nid5, data_sid, wd_nid, we_nid);

		// 			int nid6 = next_nid++;
		// 			btorf("%d or %d %d %d\n", nid6, data_sid, nid5, nid4);

		// 			int nid7 = next_nid++;
		// 			btorf("%d write %d %d %d %d\n", nid7, sid, nid_head, wa_nid, nid6);

		// 			int nid8 = next_nid++;
		// 			btorf("%d redor %d %d\n", nid8, bool_sid, we_nid);

		// 			int nid9 = next_nid++;
		// 			btorf("%d ite %d %d %d %d\n", nid9, sid, nid8, nid7, nid_head);

		// 			nid_head = nid9;
		// 		}

		// 		int nid2 = next_nid++;
		// 		btorf("%d next %d %d %d%s\n", nid2, sid, nid, nid_head, (mem->cell ? getinfo(mem->cell) :
		// getinfo(mem->mem)).c_str());

		// 		btorf_pop(stringf("next %s", log_id(mem->memid)));
		// 	}
		// }

		// while (!bad_properties.empty()) {
		// 	vector<int> todo;
		// 	bad_properties.swap(todo);

		// 	int sid = get_bv_sid(1);
		// 	int cursor = 0;

		// 	while (cursor + 1 < GetSize(todo)) {
		// 		int nid_a = todo[cursor++];
		// 		int nid_b = todo[cursor++];
		// 		int nid = next_nid++;

		// 		bad_properties.push_back(nid);
		// 		btorf("%d or %d %d %d\n", nid, sid, nid_a, nid_b);
		// 	}

		// 	if (!bad_properties.empty()) {
		// 		if (cursor < GetSize(todo))
		// 			bad_properties.push_back(todo[cursor++]);
		// 		log_assert(cursor == GetSize(todo));
		// 	} else {
		// 		int nid = next_nid++;
		// 		log_assert(cursor == 0);
		// 		log_assert(GetSize(todo) == 1);
		// 		btorf("%d bad %d\n", nid, todo[cursor]);
		// 	}
		// }

		// if (!info_filename.empty()) {
		// 	for (auto &it : info_clocks) {
		// 		switch (it.second) {
		// 		case 1:
		// 			infof("posedge %d\n", it.first);
		// 			break;
		// 		case 2:
		// 			infof("negedge %d\n", it.first);
		// 			break;
		// 		case 3:
		// 			infof("event %d\n", it.first);
		// 			break;
		// 		default:
		// 			log_abort();
		// 		}
		// 	}

		// 	std::ofstream f;
		// 	f.open(info_filename.c_str(), std::ofstream::trunc);
		// 	if (f.fail())
		// 		log_error("Can't open file `%s' for writing: %s\n", info_filename.c_str(), strerror(errno));
		// 	for (auto &it : info_lines)
		// 		f << it;
		// 	f.close();
		// }

		// if (ywmap_json.active()) {
		// 	ywmap_json.begin_object();
		// 	ywmap_json.entry("version", "Yosys Witness BTOR map");
		// 	ywmap_json.entry("generator", yosys_version_str);

		// 	ywmap_json.name("clocks");
		// 	ywmap_json.begin_array();
		// 	for (auto &entry : ywmap_clock_inputs) {
		// 		if (entry.second != 1 && entry.second != 2)
		// 			continue;
		// 		log_assert(entry.first.is_wire());
		// 		ywmap_json.begin_object();
		// 		ywmap_json.compact();
		// 		ywmap_json.entry("path", witness_path(entry.first.wire));
		// 		ywmap_json.entry("offset", entry.first.offset);
		// 		ywmap_json.entry("edge", entry.second == 1 ? "posedge" : "negedge");
		// 		ywmap_json.end_object();
		// 	}
		// 	ywmap_json.end_array();

		// 	ywmap_json.name("inputs");
		// 	ywmap_json.begin_array();
		// 	for (auto &entry : ywmap_inputs)
		// 		emit_ywmap_btor_sig(entry);
		// 	ywmap_json.end_array();

		// 	ywmap_json.name("states");
		// 	ywmap_json.begin_array();
		// 	for (auto &entry : ywmap_states)
		// 		emit_ywmap_btor_sig(entry);
		// 	ywmap_json.end_array();

		// 	ywmap_json.end_object();
		// }
	}
};

struct BtorBackend : public Backend
{
	BtorBackend() : Backend("lakeroad", "write design to egglog Lakeroad IR") {}
	void help() override
	{
		//   |---v---|---v---|---v---|---v---|---v---|---v---|---v---|---v---|---v---|---v---|
		// log("\n");
		// log("    write_btor [options] [filename]\n");
		// log("\n");
		// log("Write a BTOR description of the current design.\n");
		// log("\n");
		// log("  -v\n");
		// log("    Add comments and indentation to BTOR output file\n");
		// log("\n");
		// log("  -s\n");
		// log("    Output only a single bad property for all asserts\n");
		// log("\n");
		// log("  -c\n");
		// log("    Output cover properties using 'bad' statements instead of asserts\n");
		// log("\n");
		// log("  -i <filename>\n");
		// log("    Create additional info file with auxiliary information\n");
		// log("\n");
		// log("  -x\n");
		// log("    Output symbols for internal netnames (starting with '$')\n");
		// log("\n");
		// log("  -ywmap <filename>\n");
		// log("    Create a map file for conversion to and from Yosys witness traces\n");
		// log("\n");
	}
	void execute(std::ostream *&f, std::string filename, std::vector<std::string> args, RTLIL::Design *design) override
	{
		log_header(design, "Executing Lakeroad egglog backend.\n");

		// These passes put the design in a form that is convenient for Churchroad
		// conversion. Specifically, "piecewise" assignments like the following:
		// ```
		// assign sig[0] = ...;
		// assign sig[1] = ...;
		// assign sig[2:...] = ...;
		// ```
		// are converted to a single assignment like the following:
		// ```
		// assign sig = ...;
		// ```
		// This is necessary to prevent the bug described here:
		// https://github.com/uwsampl/churchroad/issues/13
		//
		// The `clean` pass simply removes any unused wires, see this discussion:
		// https://github.com/YosysHQ/yosys/discussions/4299
		Pass::call(design, "splice");
		Pass::call(design, "splitnets -driver");
		Pass::call(design, "clean");

		RTLIL::Module *topmod = design->top_module();

		size_t argidx = args.size();

		if (filename == "")
		{
			// The command itself is given as an arg
			if (argidx > 1 && args[argidx - 1][0] != '-')
			{
				// extra_args and friends need to see this argument.
				argidx -= 1;
				filename = args[argidx];
			}
		}
		// Has to come after other arg parsing.
		extra_args(f, filename, args, argidx);

		if (topmod == nullptr)
			log_cmd_error("No top module found.\n");

		LakeroadWorker(*f, topmod).run();

		// *f << stringf("; end of yosys output\n");
	}
} BtorBackend;

PRIVATE_NAMESPACE_END
