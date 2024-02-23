eff InsertZero {
    pub def insertZero(): List[Int32]
}

enum Tree[t] {
    case Empty,
    case Leaf(t),
    case Node(Tree[t], t, Tree[t])
}

def sum(tree: Tree[Int32]): Int32 = match tree {
    case Tree.Empty => 0,
    case Tree.Leaf(x) => x,
    case Tree.Node(l, c, r) => sum(l) + c + sum(r)
}

def insert(x: Int32, tree: Tree[Int32]): Tree[Int32] = match (x, tree) {
    case (e, Tree.Empty) =>  Tree.Leaf(e),
    case (e, Tree.Leaf(l)) => if (e < l) Tree.Node(Tree.Leaf(e), l, Tree.Empty) else Tree.Node(Tree.Empty, l, Tree.Leaf(e)),
    case (e, Tree.Node(l, c, r)) => if (e < c) Tree.Node(insert(e, l), c, r) else Tree.Node(l, c, insert(e, r))
}

def treeToList(t: Tree[Int32]): List[Int32] \ InsertZero = match t {
    case Tree.Empty => Nil,
    case Tree.Leaf(42) => List.append(42 :: Nil, do InsertZero.insertZero()),
    case Tree.Leaf(x) => x :: Nil,
    case Tree.Node(l, x, r) => List.append(List.append(treeToList(l), (x :: Nil)), treeToList(r))
}

def test(): Unit \ IO = {
    try {
        let x = insert(12, insert(1, insert(11, insert(3, insert(10, insert(42, Tree.Empty))))));
        println(treeToList(x))
    }  with InsertZero {
        def insertZero(k) = {
            k(0 :: Nil)     
        }
    }
}