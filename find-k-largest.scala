import scala.collection.mutable.PriorityQueue

def findKLargest(nums: List[Int], k: Int): Set[Int] = {
  val minHeap = PriorityQueue[Int]()(Ordering[Int].reverse)
  
  nums.foreach { num =>
    minHeap.enqueue(num)  // Add each number to the min-heap
    if minHeap.length > k then
      minHeap.dequeue()   // Remove smallest element if size exceeds k
  }
  
  minHeap.toSet
}