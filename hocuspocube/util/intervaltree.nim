# pretty much a translation of https://github.com/Ryujinx/Ryujinx/blob/master/Ryujinx.Common/Collections/IntervalTree.cs

type
    NodeColor = enum
        black
        red

    RangeNode[K, V] = object
        position: Slice[K]
        value: V

    IntervalTreeNode[K, V] = ref object
        left, right: IntervalTreeNode[K, V]
        parent {.cursor.}: IntervalTreeNode[K, V]

        position: Slice[K]
        max: K
        values: seq[RangeNode[K, V]]
        color: NodeColor

    IntervalTree*[K, V] = object
        root: IntervalTreeNode[K, V]
        count: int

proc len*[K, V](tree: IntervalTree[K, V]): int =
    tree.count

proc getNode[K, V](tree: IntervalTree[K, V], key: K): IntervalTreeNode[K, V] =
    var node = tree.root
    while node != nil:
        let cmp = cmp(key, node.position.a)
        if cmp < 0:
            node = node.left
        elif cmp > 0:
            node = node.right
        else:
            return node

    nil

proc propagateIncrease[K, V](node: IntervalTreeNode[K, V]) =
    var
        max = node.max
        cur = node

    while (cur = cur.parent; cur != nil):
        if max > cur.max:
            cur.max = max
        else:
            break

proc propagateFull[K, V](node: IntervalTreeNode[K, V]) =
    var cur = node

    while true:
        var max = cur.position.b

        if cur.left != nil and cur.left.max > max:
            max = cur.left.max

        if cur.right != nil and cur.right.max > max:
            max = cur.right.max

        cur.max = max

        cur = cur.parent
        if cur == nil:
            break

proc safeColor[K, V](node: IntervalTreeNode[K, V], color: NodeColor) =
    if node != nil: node.color = color

proc safeColor[K, V](node: IntervalTreeNode[K, V]): NodeColor =
    if node == nil: black
    else: node.color

proc safeParent[K, V](node: IntervalTreeNode[K, V]): IntervalTreeNode[K, V] =
    if node == nil: nil
    else: node.parent

proc safeLeft[K, V](node: IntervalTreeNode[K, V]): IntervalTreeNode[K, V] =
    if node == nil: nil
    else: node.left

proc safeRight[K, V](node: IntervalTreeNode[K, V]): IntervalTreeNode[K, V] =
    if node == nil: nil
    else: node.right

proc rotateLeft[K, V](tree: var IntervalTree[K, V], node: IntervalTreeNode[K, V]) =
    if node != nil:
        let right = node.safeRight
        node.right = right.safeLeft
        if node.right != nil:
            node.right.parent = node

        let nodeParent = node.safeParent
        right.parent = nodeParent
        if nodeParent == nil:
            tree.root = right
        elif node == nodeParent.safeLeft:
            nodeParent.left = right
        else:
            nodeParent.right = right

        right.left = node
        node.parent = right

        node.propagateFull()

proc rotateRight[K, V](tree: var IntervalTree[K, V], node: IntervalTreeNode[K, V]) =
    if node != nil:
        let left = node.safeLeft
        node.left = left.safeRight
        if node.left != nil:
            node.left.parent = node

        let nodeParent = node.safeParent
        left.parent = nodeParent
        if nodeParent == nil:
            tree.root = left
        elif node == nodeParent.safeRight:
            nodeParent.right = left
        else:
            nodeParent.left = left

        left.right = node
        node.parent = left

        node.propagateFull()

proc restoreBalanceAfterInsertion[K, V](tree: var IntervalTree[K, V], node: IntervalTreeNode[K, V]) =
    var node = node
    node.safeColor(red)
    while node != nil and node != tree.root and node.safeParent.safeColor == red:
        if node.safeParent == node.safeParent.safeParent.left:
            let sibling = node.safeParent.safeParent.safeRight

            if sibling.safeColor == red:
                node.safeParent.safeColor black
                sibling.safeColor black
                node.safeParent.safeParent.safeColor red
                node = node.safeParent.safeParent
            else:
                if node == node.safeParent.right:
                    node = node.safeParent
                    rotateLeft(tree, node)
                node.safeParent.safeColor black
                node.safeParent.safeParent.safeColor red
                rotateRight(tree, node.safeParent.safeParent)
        else:
            let sibling = node.safeParent.safeParent.safeLeft

            if sibling.safeColor == red:
                node.safeParent.safeColor black
                sibling.safeColor black
                node.safeParent.safeParent.safeColor red
                node = node.safeParent.safeParent
            else:
                if node == node.safeParent.left:
                    node = node.safeParent
                    rotateRight(tree, node)
                node.safeParent.safeColor black
                node.safeParent.safeParent.safeColor red
                rotateLeft(tree, node.safeParent.safeParent)

    tree.root.safeColor black

proc restoreBalanceAfterDeletion[K, V](tree: var IntervalTree[K, V], node: IntervalTreeNode[K, V]) =
    var node = node

    while node != tree.root and node.safeColor == black:
        if node == node.safeParent.safeLeft:
            var sibling = node.safeParent.safeRight

            if sibling.safeColor == red:
                sibling.safeColor black
                node.safeParent.safeColor red
                rotateLeft(tree, node.safeParent)
                sibling = node.safeParent.safeRight

            if sibling.safeLeft.safeColor == black and sibling.safeRight.safeColor == black:
                sibling.safeColor red
                node = node.safeParent
            else:
                if sibling.safeRight.safeColor == black:
                    sibling.safeLeft.safeColor black
                    sibling.safeColor red
                    rotateRight(tree, sibling)
                    sibling = node.safeParent.safeRight

                sibling.safeColor node.safeParent.safeColor
                node.safeParent.safeColor black
                sibling.safeRight.safeColor black
                rotateLeft(tree, node.safeParent)
                node = tree.root

        else:
            var sibling = node.safeParent.safeLeft

            if sibling.safeColor == red:
                sibling.safeColor black
                node.safeParent.safeColor red
                rotateRight(tree, node.safeParent)
                sibling = node.safeParent.safeLeft

            if sibling.safeRight.safeColor == black and sibling.safeLeft.safeColor == black:
                sibling.safeColor red
                node = node.safeParent
            else:
                if sibling.safeLeft.safeColor == black:
                    sibling.safeRight.safeColor black
                    sibling.safeColor red
                    rotateLeft(tree, sibling)
                    sibling = node.safeParent.safeLeft

                sibling.safeColor node.safeParent.safeColor
                node.safeParent.safeColor black
                sibling.safeLeft.safeColor black
                rotateRight(tree, node.safeParent)
                node = tree.root

proc bstInsert[K, V](tree: var IntervalTree[K, V], position: Slice[K], value: V): IntervalTreeNode[K, V] =
    var
        parent: IntervalTreeNode[K, V]
        node = tree.root

    tree.count += 1

    while node != nil:
        parent = node

        let cmp = cmp(position.a, node.position.a)
        if cmp < 0:
            node = node.left
        elif cmp > 0:
            node = node.right
        else:
            node.values.add RangeNode[K, V](position: position, value: value)

            if position.b > node.position.b:
                node.position.b = position.b
                if position.b > node.max:
                    node.max = position.b
                    node.propagateIncrease()

            return node

    let newNode = IntervalTreeNode[K, V](position: position,
        values: @[RangeNode[K, V](position: position, value: value)],
        parent: parent,
        max: position.b)
    if newNode.parent == nil:
        tree.root = newNode
    elif position.a < parent.position.a:
        parent.left = newNode
    else:
        parent.right = newNode
    newNode.propagateIncrease()

    newNode

proc max[K, V](node: IntervalTreeNode[K, V]): IntervalTreeNode[K, V] =
    var node = node
    while node.right != nil:
        node = node.right
    node

proc predecessor[K, V](node: IntervalTreeNode[K, V]): IntervalTreeNode[K, V] =
    if node.left != nil:
        max(node.left)
    else:
        var
            parent = node.parent
            node = node
        while parent != nil and node == parent.left:
            node = parent
            parent = parent.parent
        parent

proc insert*[K, V](tree: var IntervalTree[K, V], position: Slice[K], value: sink V) =
    let node = bstInsert(tree, position, value)
    restoreBalanceAfterInsertion(tree, node)

proc delete*[K, V](tree: var IntervalTree[K, V], position: Slice[K], value: V): int =
    let nodeToDelete = getNode(tree, position.a)
    if nodeToDelete == nil:
        return 0

    var
        i = 0
        numDeleted = 0
    while i < nodeToDelete.values.len:
        if nodeToDelete.values[i].position == position and nodeToDelete.values[i].value == value:
            nodeToDelete.values.del(i)
            numDeleted += 1
        else:
            i += 1

    tree.count -= numDeleted

    if nodeToDelete.values.len > 0:
        if numDeleted > 0:
            var max = nodeToDelete.values[0].position.b
            for i in 1..<nodeToDelete.values.len:
                if nodeToDelete.values[i].position.b > max:
                    max = nodeToDelete.values[i].position.b

            nodeToDelete.position.b = max

            propagateFull(nodeToDelete)
        
        return numDeleted

    var replacementNode =
        if nodeToDelete.safeLeft == nil or nodeToDelete.safeRight == nil:
            nodeToDelete
        else:
            predecessor(nodeToDelete)

    let tmp =
        if replacementNode.safeLeft != nil:
            replacementNode.safeLeft
        else:
            replacementNode.safeRight

    if tmp != nil:
        tmp.parent = replacementNode.safeParent

    if replacementNode.safeParent == nil:
        tree.root = tmp
    elif replacementNode == replacementNode.safeParent.safeLeft:
        replacementNode.safeParent.left = tmp
    else:
        replacementNode.safeParent.right = tmp

    if replacementNode != nodeToDelete:
        nodeToDelete.position = replacementNode.position
        nodeToDelete.max = replacementNode.max
        nodeToDelete.values = replacementNode.values

    propagateFull(replacementNode)

    if tmp != nil and replacementNode.safeColor == black:
        restoreBalanceAfterDeletion(tree, tmp)

    return numDeleted

iterator pairs*[K, V](tree: IntervalTree[K, V]): (Slice[K], V) =
    if tree.root != nil:
        var stack = @[tree.root]

        while stack.len > 0:
            let node = stack.pop()
            for value in node.values:
                yield (value.position, value.value)

            if node.left != nil:
                stack.add node.left
            if node.right != nil:
                stack.add node.right

iterator startingAt*[K, V](tree: IntervalTree[K, V], key: K): (Slice[K], V) =
    let node = getNode(tree, key)
    if node != nil:
        for value in node.values:
            yield (value.position, value.value)

iterator overlapping*[K, V](tree: IntervalTree[K, V], position: Slice[K]): (Slice[K], V) =
    var stack = @[tree.root]

    while stack.len > 0:
        let node = stack.pop()

        if node == nil or position.a > node.max:
            continue

        stack.add node.left

        if position.b >= node.position.a:
            if position.a <= node.position.b:
                for overlap in node.values:
                    assert overlap.position.b <= node.position.b
                    if position.a <= overlap.position.b and position.b >= overlap.position.a:
                        yield (overlap.position, overlap.value)

        stack.add node.right

when isMainModule:
    import random, sets, strformat

    var
        intervals: HashSet[(Slice[int], int)]

        tree: IntervalTree[int, int]

    proc doInsertions =
        for i in 0..<1000:
            let
                start = rand(0..100)
                ending = start + rand(1..100)
            intervals.incl((start..ending, i))
            tree.insert(start..ending, i)

    proc checkEqual =
        var checkSet: HashSet[(Slice[int], int)]
        for interval, value in pairs(tree):
            checkSet.incl((interval, value))

        assert checkSet == intervals
        assert tree.count == intervals.len

    proc doRangeIterations =
        for i in 0..<1000:
            let
                start = rand(0..50)
                ending = start + rand(1..200)

            var
                intersectSet: HashSet[(Slice[int], int)]
                bruteForceSet: HashSet[(Slice[int], int)]

            for interval, value in overlapping(tree, start..ending):
                intersectSet.incl((interval, value))

            for value in intervals:
                if value[0].a <= ending and value[0].b >= start:
                    bruteForceSet.incl(value)

            assert bruteForceSet == intersectSet, &"{i} {start}..{ending} {intersectSet - bruteForceSet} {bruteForceSet - intersectSet}"

        assert tree.count == intervals.len

    proc doDeletions =
        let targetCount = intervals.len div 2
        while intervals.len > targetCount:
            let toBeRemoved = intervals.pop()
            assert tree.delete(toBeRemoved[0], toBeRemoved[1]) == 1
        assert tree.count == targetCount

    proc testStartingAt =
        for i in 0..<100:
            var
                bruteforceStarting: HashSet[(Slice[int], int)]
                startingAtStarting: HashSet[(Slice[int], int)]
            for key, value in pairs tree:
                if key.a == i:
                    bruteforceStarting.incl((key, value))
            for key, value in startingAt(tree, i):
                startingAtStarting.incl((key, value))
            assert bruteforceStarting == startingAtStarting

    for i in 0..<5:
        doInsertions()
        checkEqual()
        doRangeIterations()
        testStartingAt()
        doDeletions()
        checkEqual()
        doRangeIterations()
        testStartingAt()
