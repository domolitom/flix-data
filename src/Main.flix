enum Tree {
    case Empty,
    case Leaf(Int32),
    case Node(Tree, Int32, Tree)
}

def sum(t: Tree): Int32 = match t {
    case Tree.Empty => 0,
    case Tree.Leaf(x) => x,
    case Tree.Node(l, c, r) => sum(l) + c + sum(r)
}

def insert(x: Int32, t: Tree): Tree = match (x, t) {
    case (e, Tree.Empty) =>  Tree.Leaf(e),
    case (e, Tree.Leaf(l)) => if (e < x) Tree.Node(Tree.Leaf(e), l, Tree.Empty) else Tree.Node(Tree.Empty, l, Tree.Leaf(e)),
    case (e, Tree.Node(l, c, r)) => if (e < c) Tree.Node(insert(e, l), c, r) else Tree.Node(l, c, insert(e, r))
}

def treeToList(t: Tree): List[Int32] = match t {
    case Tree.Empty => Nil,
    case Tree.Leaf(x) => x :: Nil,
    case Tree.Node(l, x, r) => List.append(List.append(treeToList(l), (x :: Nil)), treeToList(r))
}

def test(): Unit \ IO = {
    let x = insert(2, insert(3, insert(6, insert(10, Tree.Empty))));
    println(treeToList(x))
}