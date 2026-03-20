(* howMany: counts the number of occurrences of element e in list l
   arguments: e - element to search for, l - list
   returns: int - number of times e appears in the list *)
let rec howMany e l= 
	if l = [] then 0
	else 
		let head = List.hd l in
		let tail = List.tl l in

		if head = e
			then 1 +  howMany e tail
		else
			howMany e tail

(* delete: removes all occurrences of element e from list l
   arguments: e - element to delete, l - list
   returns: 'a list - new list with all occurrences of e removed *)
let rec delete e l = 
	if l = [] then []
	else
		let head = List.hd l in
		let tail = List.tl l in
		
		if e = head then
			delete e tail
		else
			head :: (delete e tail)

(* sum: calculates the sum of a float list
   arguments: l - list of floats
   returns: float - sum of all elements in the list *)
let rec sum l =
	if l = [] then 0.0
	else
		let head = List.hd l in
		let tail = List.tl l in 
		head +. (sum tail)

(* length: calculates the length of a list
   arguments: l - list
   returns: float - length of the list (as a float) *)
let rec length l = 
	if l = [] then 0.0
	else 
		let tail = List.tl l in 
		1.0 +. length tail
(* mean: calculates the mean (average) of a list
   arguments: l - list of floats
   returns: float - average of all elements in the list *)
let rec mean l = 
	if l = [] then 0.0
	
	else 
		(sum l)/. (length l)