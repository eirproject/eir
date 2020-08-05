:ok = File.write("int_tuple.etf", :erlang.term_to_binary({1, 2, 3}))
:ok = File.write("test_1.etf", :erlang.term_to_binary([:hello, 1, [], 2222]))
:ok = File.write("test_2.etf", :erlang.term_to_binary(%{"foo" => :bar, 1 => {}}))
