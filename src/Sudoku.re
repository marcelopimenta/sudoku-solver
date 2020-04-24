
let line_contains = (board, line, value) => List.exists((element) => element == value, List.nth(board,line));

let column_contains = (board, column, value) => {
    let check_list = List.map((line) => List.nth(line, column) == value, board);
    List.exists((element) => element == true, check_list)
}

let rec range = (start_:int, end_:int) => {
    // range(1,4) -> [1,2,3,4]
    if (start_> end_) {
        [];
    }
    else {
        [start_, ...range(start_+1, end_)];
    }
 }
  
let get_board_values = (board, line_start, line_end, column_start, column_end) => {
    let l = range(line_start, line_end);
    let lines = List.sort((a,b)=>a-b, List.append(List.append(l,l),l));
    let c = range(column_start, column_end);
    let columns = List.append(List.append(c,c),c);
    List.map2((l,c) => List.nth(List.nth(board,l), c),lines,columns)
 }
 
 let get_column_values = (board, column) => {
     let lines = range(0,8);
     List.map((line) => List.nth(List.nth(board, line), column), lines);
 }

 let get_quad_values = (board_,line,column) => {
     if (line < 3 && column < 3) {
         get_board_values(board_, 0, 2, 0, 2);
     }
     else if (line < 3 && column < 6) {
         get_board_values(board_, 0, 2, 3, 5);
     }
     else if (line < 3 && column < 9) {
         get_board_values(board_, 0, 2, 6, 8);
     }
     else if (line < 6 && column < 3) {
         get_board_values(board_, 3, 5, 0, 2);
     }
     else if (line < 6 && column < 6) {
         get_board_values(board_, 3, 5, 3, 5);
     }
     else if (line < 6 && column < 9) {
         get_board_values(board_, 3, 5, 6, 8);
     }
     else if (line < 9 && column < 3) {
         get_board_values(board_, 6, 8, 0, 2);
     }
     else if (line < 9 && column < 6) {
         get_board_values(board_, 6, 8, 3, 5);
     }
     else if (line < 9 && column < 9) {
         get_board_values(board_, 6, 8, 6, 8);
     }
     else {
         []
     }
}

let quad_contains = (board, line, column, value) => {
   List.exists((e) => e == value, get_quad_values(board,line,column));
}

let can_place = (board, line, column, value) => {
    !line_contains(board, line, value) && !column_contains(board, column, value) && !quad_contains(board, line, column, value);
}

let is_done = (board) => {
    let check_list = List.map((line) => List.exists((element) => element == 0, line), board);
    !List.exists((element) => element == true, check_list)
}

let rec find_index = (list, value, index) => {
   if (index >= List.length(list)) {
     -1
   } else { 
     if (List.nth(list, index) == value) { 
        index
     } else { 
        find_index(list, value, index+1)
     }
   }
}

let replace = (list, index, value) => {
    List.mapi((i,a) => if (i == index){value} else{a}, list)
}

let get_possible_values = (board, line, column) => {
   if (line >= List.length(board) || line <0) {
       [];
   }
   else {
    let values = [1,2,3,4,5,6,7,8,9];
    let line_values = List.filter((value) => !line_contains(board, line, value), values);
    let column_values = List.filter((value) => !column_contains(board, column, value), line_values);
    List.filter((value) => !quad_contains(board, line, column, value), column_values);
   }
}

let find_empty_column = (board, line) => find_index(List.nth(board, line), 0, 0);

let rec find_empty = (board, line) => {
    if (line >= List.length(board)) {
        (-1,-1)
    }
    else {
        let index = find_index(List.nth(board, line), 0, 0);
        if (index >= 0 && line < List.length(board)) {
            (line, index)
        }
        else if (index >= 0 && line >= List.length(board)){
            (0, index)
        }
        else {
            find_empty(board, line+1)
        }
    }
}

let show_board = (board, depth) => {
    let spaces = List.fold_left((acc,_)=> acc ++ " "," ",range(0,depth+4));
    Js.log(spaces ++ "-------------------");
    List.iter((line) => {
        let map = List.map((item) => string_of_int(item), line);
        Js.log(spaces ++ List.fold_left((acc, element) => acc ++ " " ++ element, "", map))
    }, board);
 }

let show_list = (list) => {
    let map = List.map((item) => string_of_int(item), list);
    List.fold_left((acc, element) => acc ++ " " ++ element, "", map);
}

let rec solve = (board, line, column, possible_values, depth) => {
    if (is_done(board)) {
        (true,board);
    }
    else {
        if (List.length(possible_values) > 0) {
            let new_board_line = replace(List.nth(board, line), column, List.hd(possible_values));
            let new_board = replace(board, line, new_board_line);
            let (next_line, next_column) = find_empty(new_board, line);
            let new_possible_values = get_possible_values(new_board, next_line, next_column);
            let (solved,solved_board) = solve(new_board, next_line, next_column, new_possible_values,depth+1);
            if (solved) {
                (true,solved_board);
            } else {
                solve(board, line, column, List.tl(possible_values), depth+1);
            }
        }
        else {
            (false,[]);
        }
    }
}

let solve_puzzle = (board) => {
   let (line, column) = find_empty(board, 0);
   let possible_values = get_possible_values(board, line, column);
   let (_,result) = solve(board, line, column, possible_values, 0);
   show_board(result,0);
}

let run = () => {
    range(1,3)
    |> List.iter((n)=>{
        let b = Node.Fs.readFileAsUtf8Sync("games/game"++ string_of_int(n) ++".txt")
        |> Js.String.split("\n")
        |> Array.to_list
        |> List.map((line) => Array.to_list(Js.String.splitByRe([%re "/[\\s]+/"], Js.String.trim(line))))
        |> List.map((line) => List.map((item) =>{
            switch(item){
            | Some(n) =>  int_of_string(Js.String.trim(n))
            | None => -1  
            }
           } ,line));
        solve_puzzle(b);
    });
}

run();

