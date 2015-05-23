class Main inherits IO {
   main(): SELF_TYPE { foo(new B) };

   foo( x : B) : SELF_TYPE {{
   	 out_int(x.bar());
	 out_string("\n");
   }};

};

class Foo {
      x : Int;
};

