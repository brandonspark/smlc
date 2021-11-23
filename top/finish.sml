
(* I stole all this code from StackOverflow. *)
structure Popen :>
      sig
          (* Parent wants to write to stdin, read stdout, or read stdout + stderr *)
          datatype pipe_type = PIPE_W | PIPE_R | PIPE_RE
          val popen : string * pipe_type -> Posix.IO.file_desc
          val pclose : Posix.IO.file_desc -> Posix.Process.exit_status option
      end =
struct

datatype pipe_type = PIPE_W | PIPE_R | PIPE_RE

type pinfo = { fd : Posix.ProcEnv.file_desc, pid : Posix.Process.pid }

val pids : pinfo list ref = ref []

(* Implements popen(3) *)
fun popen (cmd, t) =
  let val { infd = readfd, outfd = writefd } = Posix.IO.pipe ()
  in case (Posix.Process.fork (), t)
      of (NONE, t) => (* Child *)
     (( case t
         of PIPE_W => Posix.IO.dup2 { old = readfd, new = Posix.FileSys.stdin }
          | PIPE_R => Posix.IO.dup2 { old = writefd, new = Posix.FileSys.stdout }
          | PIPE_RE => ( Posix.IO.dup2 { old = writefd, new = Posix.FileSys.stdout }
                       ; Posix.IO.dup2 { old = writefd, new = Posix.FileSys.stderr })
      ; Posix.IO.close writefd
      ; Posix.IO.close readfd
      ; Posix.Process.execp ("/bin/sh", ["sh", "-c", cmd]))
      handle OS.SysErr (err, _) =>
             ( print ("Fatal error in child: " ^ err ^ "\n")
             ; OS.Process.exit OS.Process.failure ))
       | (SOME pid, t) => (* Parent *)
     let val fd = case t of PIPE_W => (Posix.IO.close readfd; writefd)
                          | PIPE_R => (Posix.IO.close writefd; readfd)
                          | PIPE_RE => (Posix.IO.close writefd; readfd)
         val _ = pids := ({ fd = fd, pid = pid } :: !pids)
     in fd end
  end

(* Implements pclose(3) *)
fun pclose (fd : Posix.IO.file_desc) =
  case List.partition (fn { fd = f, pid = _ } => f = fd) (!pids)
   of ([], _) => NONE
    | ([{ fd = _, pid = pid }], pids') =>
      let val _ = pids := pids'
      val (_, status) = Posix.Process.waitpid (Posix.Process.W_CHILD pid, [])
      val _ = Posix.IO.close fd
      in SOME status end
    | _ => raise Bind (* This should be impossible. *)
end

(* Here's my shitty code. *)
structure Finish =
  struct

    infix |>
    fun x |> f = f x

    fun endline s = s ^ "\n"

    fun stringToWord8Vector s =
      let
        val ws =
             s
          |> String.explode
          |> List.map Char.ord
          |> List.map Word8.fromInt
      in
        Word8VectorSlice.full (
            Word8Vector.tabulate
              ( List.length ws
              , fn i => List.nth (ws, i)
              )
          )
      end

    fun translate s =
         s
      |> Parser.parse_repl
      |> (fn Syntax.Dprog _ => raise Fail "impossible"
           | Syntax.Drepl toprepl => toprepl)
      |> Syntax.out_top
      |> endline

    fun if_exec_exit b f =
      if b then (
        f ();
        OS.Process.exit 0;
        ()
        )
      else
        ()

    (* Really shitty random number hack, because we start a new process every
     * time --favorite-ta is called...
     *)
    val seed =
      let
        val randInt = Int.fromLarge (Time.toMilliseconds (Time.now ())) mod 196613
      in
        Random.rand
          ( (randInt * randInt * randInt) mod 196613
          , (randInt * randInt) mod 196613
          )
      end

    val favorite_tas =
      [ "Audacious Allen"
      , "Dashing Disha"
      , "Effervescent Ethan"
      , "Extreme Eunice"
      , "Intelligent Isabel"
      , "Juicy Jon"
      , "Jubilant Jonathan"
      , "Jaunty Juhi"
      , "Joyful Julia"
      , "Jazzy Justin"
      , "Kind Keiffer"
      , "Knowledgeable Kaz"
      , "Luscious Leah"
      , "Magical Megha"
      , "Notorious Nikki"
      , "Refreshing Rachel"
      , "Resilient Runming"
      , "Sincere Sanjana"
      , "Sassy Sonya"
      , "Spectacular Stefan"
      , "Sharp Steven"
      , "Scintillating Surabhi"
      , "Thoughtful Thea"
      , "Wonderful Will"
      ]

    fun run () =
      let
        val args = CommandLine.arguments ()
        (* Command-line options! *)
        val _ =
          if_exec_exit (List.exists (fn s => s = "-h") args)
            (fn () => OS.Process.system "cat ~/Projects/smlc/assets/fakehelp.txt >&2")
        val _ =
          if_exec_exit (List.exists (fn s => s = "--special-help") args)
            (fn () => OS.Process.system "cat ~/Projects/smlc/assets/fakespecialhelp.txt >&2")
        val _ =
          if_exec_exit (List.exists (fn s => s = "--steve") args)
            (fn () => OS.Process.system "cat ~/Projects/smlc/assets/steve.txt >&2")
        val _ =
          if_exec_exit (List.exists (fn s => s = "--companies") args)
            (fn () => TextIO.output (TextIO.stdErr,
              "Standard ML (SML) is a general-purpose modular functional"
              ^ " programming language \nwith compile-time type checking and type"
              ^ " inference.\nHere is a comprehensive listing of all industry locations"
              ^ " currently using Standard ML:\n\n"))
        val _ =
          if_exec_exit (List.exists (fn s => s = "--favorite-ta") args)
            (fn () => TextIO.output (TextIO.stdErr,
              List.nth
                ( favorite_tas
                , Random.randRange (0, List.length favorite_tas - 1) seed
                )
             )
             )
        val _ =
          if_exec_exit (List.exists (fn s => s = "--why-is-150-better-than-122") args)
            (fn () => ( TextIO.output
                      (TextIO.stdErr, "Segmentation fault (core dumped)");
                      OS.Process.exit 1
                    )
            )
        val reveal = List.exists (fn s => s = "--reveal") args
        val out_fn =
          if List.exists (fn s => s = "--c_syntax=true") args then (
            fn s =>
              ( (if reveal then TextIO.output (TextIO.stdErr, "REVEAL " ^ translate s)
                else ())
              ; stringToWord8Vector (translate s)
              )
            )
          else
            fn s => stringToWord8Vector s
        val opt_file =
          List.find (fn s => Option.isSome (OS.Path.ext s)) args
        val fd = Popen.popen ("sml >&2", Popen.PIPE_W)
      in
        (* Load in file, if it exists. *)
        case
          List.find (fn s => Option.isSome (OS.Path.ext s)) args
        of
          NONE => ()
        | SOME file => (
            let
              val decs =
                case Parser.parse_prog file of
                  Syntax.Drepl _ => raise Fail "impossible"
                | Syntax.Dprog decs => decs
            in
              TextIO.output (TextIO.stdErr, "[opening " ^ file ^ "]\n");
              List.foldl
                (fn (dec, ()) =>
                  let
                    val msg = Syntax.out_dec dec ^ "\n"
                  in
                    if reveal then
                      TextIO.output (TextIO.stdErr, "REVEAL " ^ msg)
                    else
                      ();
                    Posix.IO.writeVec
                      (fd, stringToWord8Vector msg)
                    |> ignore
                  end
                )
                ()
                decs
            end
            |> ignore
            );
        (* Main loop to simulate REPL. *)
        while true do (
          case TextIO.inputLine TextIO.stdIn of
            NONE => raise Fail "no input"
          | SOME "\n" => (
              Posix.IO.writeVec
                (fd, stringToWord8Vector "\n")
              |> ignore
            )
          | SOME s => (
              Posix.IO.writeVec (fd, out_fn s)
              |> ignore
            )
        )
      end
end

val _ = Finish.run ()
