churchroad.so: churchroad.cc
	$(CXX) $(shell yosys-config --cxxflags --ldflags) -shared -o $@ churchroad.cc -lboost_filesystem

clean:
	rm -rfv *.d *.o churchroad.so*

-include *.d
