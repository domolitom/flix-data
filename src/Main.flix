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