int x = 2;

int y = x + 2;

int double (int n) {
  /*@requires n >= 0           */
  /*@ensures double n ~= n + n */
  return n + n;
};



int fact (int n) {
  return
    switch n {
      case 0:
        1
      case _:
        n * fact (n - 1)
    }
  ;
};

int length ('a list l) {
  return
    switch l {
      case []:
        0
      case x::xs:
        1 + length xs
    }
  ;
};

(int, int) list zip (int list l1, int list l2) {
  return
    switch (l1, l2) {
      case ([], _):
        []
      case (_, []):
        []
      case (x::xs, y::ys):
        (x,y) :: zip (xs, ys)
    }
  ;
};

('a, 'b) swap ('b x, 'a y) {
  return (y, x);
};

(int, int) divmod (int n, int d) {
  return
    switch n < d {
      case true:
        (0, n)
      case false:
        { (int, int) qd = divmod (n - d, d);
          return (#1 qd + 1, #2 qd);
        }
    };
};

'a identity ('a x) {
  return x;
};

'b list map (('a -> 'b) f, 'a list l) {
  return
    switch l {
      case []:
        []
      case x::xs:
        f x :: map (f, xs)
    }
  ;
};

'a factCPS (int n, (int -> 'a) k) {
  return
    switch n {
      case 0:
        k 1
      case _:
        { 'a k' (int res) {
            return k (res * n);
          };
          return factCPS (n - 1, k');
        }
    };
};

