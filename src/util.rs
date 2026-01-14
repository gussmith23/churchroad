use egraph_serialize::{ClassId, EGraph, NodeId};

/// - number_of_children: depth into the tree to display. Should be renamed.
pub fn display_enode_serialized(
    egraph: &EGraph,
    node_id: &NodeId,
    number_of_children: usize,
) -> String {
    let this_node = match egraph[node_id].op.as_str() {
        op @ ("Op0" | "Op1" | "Op2" | "Op3") => {
            format!("{} {}", op, egraph[&egraph[node_id].children[0]].op)
        }
        op => op.to_owned(),
    };

    if number_of_children == 0 {
        format!("{}: {}", node_id, this_node)
    } else {
        let children = egraph[node_id]
            .children
            .iter()
            .map(|child_id| display_enode_serialized(egraph, child_id, number_of_children - 1))
            .collect::<Vec<String>>()
            .join(", ");
        format!("{}: {}({})", node_id, this_node, children)
    }
}

pub fn missing_hastype(egraph: &EGraph) -> Vec<ClassId> {
    let classes_with_hastype = egraph
        .nodes
        .iter()
        .filter_map(|(_, node)| {
            if node.op == "HasType" {
                Some(egraph[&node.children[0]].eclass.clone())
            } else {
                None
            }
        })
        .collect::<Vec<_>>();
    let classes_without_hastype: Vec<_> = egraph
        .classes()
        .iter()
        .filter_map(|(class_id, class)| {
            // Ignore a bunch of types.
            // TODO(@gussmith23): This is a pretty hacky way of checking types.
            if !class.id.to_string().starts_with("Expr") {
                return None;
            }

            if !classes_with_hastype.contains(class_id) {
                Some(class_id.clone())
            } else {
                None
            }
        })
        .collect();

    classes_without_hastype
}
