churchroad.so: churchroad.cc
	$(CXX) $(shell yosys-config --cxxflags --ldflags) -shared -o $@ churchroad.cc

clean:
	rm -rfv *.d *.o churchroad.so*

-include *.d
