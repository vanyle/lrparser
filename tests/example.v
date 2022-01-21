let g = 56;
let print_stuff = function(a){
    print(a);
    if(a)(a+print_stuff(a - 1))(0)
};
let b = print_stuff(10);
print(g * 10)