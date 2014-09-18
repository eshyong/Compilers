     1	(* models one-dimensional cellular automaton on a circle of finite radius
     2	   arrays are faked as Strings,
     3	   X's respresent live cells, dots represent dead cells,
     4	   no error checking is done *)
     5	class CellularAutomaton inherits IO {
     6	    population_map : String;
     7	   
     8	    init(map : String) : SELF_TYPE {
     9	        {
    10	            population_map <- map;
    11	            self;
    12	        }
    13	    };
    14	   
    15	    print() : SELF_TYPE {
    16	        {
    17	            out_string(population_map.concat("\n"));
    18	            self;
    19	        }
    20	    };
    21	   
    22	    num_cells() : Int {
    23	        population_map.length()
    24	    };
    25	   
    26	    cell(position : Int) : String {
    27	        population_map.substr(position, 1)
    28	    };
    29	   
    30	    cell_left_neighbor(position : Int) : String {
    31	        if position = 0 then
    32	            cell(num_cells() - 1)
    33	        else
    34	            cell(position - 1)
    35	        fi
    36	    };
    37	   
    38	    cell_right_neighbor(position : Int) : String {
    39	        if position = num_cells() - 1 then
    40	            cell(0)
    41	        else
    42	            cell(position + 1)
    43	        fi
    44	    };
    45	   
    46	    (* a cell will live if exactly 1 of itself and it's immediate
    47	       neighbors are alive *)
    48	    cell_at_next_evolution(position : Int) : String {
    49	        if (if cell(position) = "X" then 1 else 0 fi
    50	            + if cell_left_neighbor(position) = "X" then 1 else 0 fi
    51	            + if cell_right_neighbor(position) = "X" then 1 else 0 fi
    52	            = 1)
    53	        then
    54	            "X"
    55	        else
    56	            '.'
    57	        fi
    58	    };
    59	   
    60	    evolve() : SELF_TYPE {
    61	        (let position : Int in
    62	        (let num : Int <- num_cells[] in
    63	        (let temp : String in
    64	            {
    65	                while position < num loop
    66	                    {
    67	                        temp <- temp.concat(cell_at_next_evolution(position));
    68	                        position <- position + 1;
    69	                    }
    70	                pool;
    71	                population_map <- temp;
    72	                self;
    73	            }
    74	        ) ) )
    75	    };
    76	};
    77	
    78	class Main {
    79	    cells : CellularAutomaton;
    80	   
    81	    main() : SELF_TYPE {
    82	        {
    83	            cells <- (new CellularAutomaton).init("         X         ");
    84	            cells.print();
    85	            (let countdown : Int <- 20 in
    86	                while countdown > 0 loop
    87	                    {
    88	                        cells.evolve();
    89	                        cells.print();
    90	                        countdown <- countdown - 1;
    91	                    
    92	                pool
    93	            );  (* end let countdown
    94	            self;
    95	        }
    96	    };
    97	};
