# A far simpler Mirage intro 

A simpler approach to introducing Mirage concepts.  From Oleg, modified by Anil, WIP from [Shonan 143](https://github.com/avsm/shonan-143)

```ocaml
# let hello () =
    let rec loop n =
      match n with
      | 0 -> ()
      | _ ->
        prerr_endline "Hello";
        Unix.sleep 1;
        loop (n-1)
    in loop 3
val hello : unit -> unit = <fun>
```

Just the mention of `prerr_endline` brings all of the standard input, and
`sleep` brings in the rest of Unix

```ocaml
# let hello (print, sleep) =
    let rec loop n =
      match n with
      | 0 -> ()
      | _ ->
        print "Hello";
        sleep 1;
        loop (n-1)
  in loop 3
val hello : (string -> 'a) * (int -> 'b) -> unit = <fun>
```

It can be used as follows:

```ocaml
# hello (prerr_endline, Unix.sleep)
Hello
Hello
Hello
- : unit = ()
```

or even with spinning:

```ocaml
# let spinsleep n =
    for i = 0 to 10 * n do
     ignore (Sys.opaque_identity (sin (1.0)))
    done
val spinsleep : int -> unit = <fun>
# hello (prerr_endline, spinsleep)
Hello
Hello
Hello
- : unit = ()
```

The earlier type of hello:

```
val hello : (string -> 'a) * (int -> 'b) -> 'c = <fun>
```

looked a bit weird, with three unrelated type variables. 

We haven't accounted for
the fact that print and sleep are effects. They are not pure functions
that the above signature makes them to be.
So, let's be honest and say explicitly that 
print/sleep are making/producing/building
some effects. We also need a way to combine effectful computations.

In simpler terms, we have to program/abstract over the semicolon.

```ocaml
# let hello ((>>),print,sleep) =
    let rec loop n =
      match n with
      | 0 -> sleep 0
      | n ->
         print "Hello" >>
         sleep 1 >>
         loop (n-1)
   in loop 3
val hello : ('a -> 'a -> 'a) * (string -> 'a) * (int -> 'a) -> 'a = <fun>
```
Now the signature looks realistic and we can do the earlier Unixy hello.

```ocaml
# hello ((fun x y -> ignore x; y), prerr_endline, Unix.sleep)
Hello
Hello
Hello
- : unit = ()
```

And we can do something else like lwt:

```ocaml
# #require "lwt"
# hello (Lwt.(<?>), prerr_endline, Lwt.sleep)
Characters 18-31:
Error: This expression has type string -> unit
       but an expression was expected of type string -> 'a Lwt.t
       Type unit is not compatible with type 'a Lwt.t 
```

We already have three arguments, for such a simple program.
It quickly gets out of hand. We need a better naming:
keyword-like arguments. Also, we will soon need to group the
related arguments as there would be a lot of them.
Thus, we need groups of keyword argumnents: in other words, modules
(or records)

The basic module of some OS-like computations, and the way to
combine them.  Wouldn't it be good if `;` were a binary operator?

```ocaml
# module type DoIt = sig
   type 'a io
   val (>>) : 'a io -> unit io -> unit io
   val forever : (unit -> unit io) -> unit io
  end
module type DoIt =
  sig
    type 'a io
    val ( >> ) : 'a io -> unit io -> unit io
    val forever : (unit -> unit io) -> unit io
  end

# module type Console = sig
   include DoIt
   val print: string -> unit io
  end
module type Console =
  sig
    type 'a io
    val ( >> ) : 'a io -> unit io -> unit io
    val forever : (unit -> unit io) -> unit io
    val print : string -> unit io
  end

# module type Time = sig
   include DoIt
   val sleep: int -> unit io
  end
module type Time =
  sig
    type 'a io
    val ( >> ) : 'a io -> unit io -> unit io
    val forever : (unit -> unit io) -> unit io
    val sleep : int -> unit io
  end
```

The sharing constraint says we deal with the same io type 

```ocaml
# module Hello (C:Console)
               (T:Time with type 'a io = 'a C.io) 
              = struct
   open C
   open T

   let hello () =
     let rec loop  =
       print "Hello" >>
       sleep 1 >>
       loop ()
     in loop ()

  end
module Hello :
  functor
    (C : Console) (T : sig
                         type 'a io = 'a C.io
                         val ( >> ) : 'a io -> unit io -> unit io
                         val forever : (unit -> unit io) -> unit io
                         val sleep : int -> unit io
                       end) ->
    sig val hello : unit -> unit T.io end
```

Here is how we can use it:

```ocaml
module DoItUnix  = struct
 type 'a io = 'a
 let (>>) x y = ignore x; y
end

let test () =
 let module C = struct include DoItUnix let print = prerr_endline end in
 let module T = struct include DoItUnix let sleep = Unix.sleep end in
 let module M = Hello(C)(T) in
 M.hello ()
```

A bit of more abstraction: if we mean run forever, it would be
better expressed as a combinator.

```ocaml
module type DoIt = sig
 type 'a io
 val (>>) : 'a io -> unit io -> unit io
 (* I could have used forever: unit io -> unit io  
    signature, making 'a io more complicated instead...
    There are lots of design choices
  *)      
 val forever : (unit -> unit io) -> unit io
end

(* Extending the basic OS interface, in two different ways *)
module type Console = sig
 include DoIt
 val print: string -> unit io
end

module type Time = sig
 include DoIt
 val sleep: int -> unit io
end

(* The sharing constraint says we deal with the same io type *)
module Hello(C:Console)(T:Time with type 'a io = 'a C.io) = struct
 open C
 open T

 let hello () =
   forever @@ fun () -> 
     print "Hello" >>
     sleep 1

end
```

Here is how we can use it:

```
module DoItUnix  = struct
 type 'a io = 'a
 let (>>) x y = ignore x; y
 let forever th = while true do th () done
end

let test () =
 let module C = struct include DoItUnix let print = prerr_endline end in
 let module T = struct include DoItUnix let sleep = Unix.sleep end in
 let module M = Hello(C)(T) in
 M.hello ()
```

But we can also do something else.
Please do note that 'a code IS NOT a monad. It is not even applicative!

```ocaml
module DoItUnixC  = struct
 type 'a io = 'a code
 let (>>) x y = .<.~x; .~y>.
 let forever th = .<while true do .~(th ()) done>.
end

let test () =
 let module C = struct 
   include DoItUnixC 
   let print x = .<prerr_endline x>. 
 end in
 let module T = struct 
   include DoItUnixC
   let sleep x = .<Unix.sleep x>. 
 end in
 let module M = Hello(C)(T) in
 M.hello ()

(*
val test : unit -> unit code = <fun>
*)

let _ = test ()
(*
- : unit code = .<while true do Stdlib.prerr_endline "Hello"; Unix.sleep 1 done>. 
*)
```


