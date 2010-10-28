package scala.collection.parallel
package mutable




import collection.generic._
import collection.mutable.DefaultEntry
import collection.mutable.HashEntry
import collection.mutable.HashTable



class ParHashMap[K, V] private[collection] (contents: HashTable.Contents[K, DefaultEntry[K, V]])
extends ParMap[K, V]
   with GenericParMapTemplate[K, V, ParHashMap]
   with ParMapLike[K, V, ParHashMap[K, V], collection.mutable.HashMap[K, V]]
   with ParHashTable[K, DefaultEntry[K, V]]
{
self =>
  initWithContents(contents)
  
  type Entry = collection.mutable.DefaultEntry[K, V]
  
  def this() = this(null)
  
  override def mapCompanion: GenericParMapCompanion[ParHashMap] = ParHashMap
  
  override def empty: ParHashMap[K, V] = new ParHashMap[K, V]
  
  protected[this] override def newCombiner = ParHashMapCombiner[K, V]
  
  def seq = new collection.mutable.HashMap[K, V](hashTableContents)
  
  def parallelIterator = new ParHashMapIterator(1, table.length, size, table(0).asInstanceOf[DefaultEntry[K, V]]) with SCPI
  
  override def size = tableSize
  
  def get(key: K): Option[V] = {
    val e = findEntry(key)
    if (e == null) None
    else Some(e.value)
  }
  
  override def put(key: K, value: V): Option[V] = {
    val e = findEntry(key)
    if (e == null) { addEntry(new Entry(key, value)); None }
    else { val v = e.value; e.value = value; Some(v) }
  }
  
  override def update(key: K, value: V): Unit = put(key, value)
  
  override def remove(key: K): Option[V] = {
    val e = removeEntry(key)
    if (e ne null) Some(e.value)
    else None
  }
  
  def += (kv: (K, V)): this.type = { 
    val e = findEntry(kv._1)
    if (e == null) addEntry(new Entry(kv._1, kv._2))
    else e.value = kv._2
    this
  }
  
  def -=(key: K): this.type = { removeEntry(key); this }
  
  type SCPI = SignalContextPassingIterator[ParHashMapIterator]
  
  class ParHashMapIterator(start: Int, untilIdx: Int, totalSize: Int, e: DefaultEntry[K, V])
  extends EntryIterator[(K, V), ParHashMapIterator](start, untilIdx, totalSize, e) with ParIterator {
  me: SCPI =>
    def entry2item(entry: DefaultEntry[K, V]) = (entry.key, entry.value);
    def newIterator(idxFrom: Int, idxUntil: Int, totalSz: Int, es: DefaultEntry[K, V]) =
      new ParHashMapIterator(idxFrom, idxUntil, totalSz, es) with SCPI
  }
  
  private[parallel] override def brokenInvariants = {
    // bucket by bucket, count elements
    val buckets = for (i <- 0 until (table.length / sizeMapBucketSize)) yield checkBucket(i)
    
    // check if each element is in the position corresponding to its key
    val elems = for (i <- 0 until table.length) yield checkEntry(i)
    
    buckets.flatMap(x => x) ++ elems.flatMap(x => x)
  }
  
  private def checkBucket(i: Int) = {
    def count(e: HashEntry[K, DefaultEntry[K, V]]): Int = if (e eq null) 0 else 1 + count(e.next)
    val expected = sizemap(i)
    val found = ((i * sizeMapBucketSize) until ((i + 1) * sizeMapBucketSize)).foldLeft(0) {
      (acc, c) => acc + count(table(c))
    }
    if (found != expected) List("Found " + found + " elements, while sizemap showed " + expected)
    else Nil
  }
  
  private def checkEntry(i: Int) = {
    def check(e: HashEntry[K, DefaultEntry[K, V]]): List[String] = if (e eq null) Nil else
      if (index(elemHashCode(e.key)) == i) check(e.next)
      else ("Element " + e.key + " at " + i + " with " + elemHashCode(e.key) + " maps to " + index(elemHashCode(e.key))) :: check(e.next)
    check(table(i))
  }
  
}


object ParHashMap extends ParMapFactory[ParHashMap] {
  def empty[K, V]: ParHashMap[K, V] = new ParHashMap[K, V]
  
  def newCombiner[K, V]: Combiner[(K, V), ParHashMap[K, V]] = ParHashMapCombiner.apply[K, V]
  
  implicit def canBuildFrom[K, V]: CanCombineFrom[Coll, (K, V), ParHashMap[K, V]] = new CanCombineFromMap[K, V]
}


private[mutable] abstract class ParHashMapCombiner[K, V](private val tableLoadFactor: Int)
extends collection.parallel.BucketCombiner[(K, V), ParHashMap[K, V], DefaultEntry[K, V], ParHashMapCombiner[K, V]](ParHashMapCombiner.numblocks)
   with collection.mutable.HashTable.HashUtils[K]
{
self: EnvironmentPassingCombiner[(K, V), ParHashMap[K, V]] =>
  private var mask = ParHashMapCombiner.discriminantmask
  private var nonmasklen = ParHashMapCombiner.nonmasklength
  
  def +=(elem: (K, V)) = {
    sz += 1
    val hc = improve(elemHashCode(elem._1))
    val pos = (hc >>> nonmasklen)
    if (lasts(pos) eq null) {
      // initialize bucket
      heads(pos) = new Unrolled[DefaultEntry[K, V]]
      lasts(pos) = heads(pos)
    }
    // add to bucket
    lasts(pos) = lasts(pos).add(new DefaultEntry(elem._1, elem._2))
    this
  }
  
  def result: ParHashMap[K, V] = if (size >= (ParHashMapCombiner.numblocks * sizeMapBucketSize)) { // 1024
    // construct table
    val table = new AddingHashTable(size, tableLoadFactor)
    val insertcount = executeAndWaitResult(new FillBlocks(heads, table, 0, ParHashMapCombiner.numblocks))
    table.setSize(insertcount)
    // TODO compare insertcount and size to see if compression is needed
    val c = table.hashTableContents
    new ParHashMap(c)
  } else {
    // construct a normal table and fill it sequentially
    val table = new HashTable[K, DefaultEntry[K, V]] {
      def insertEntry(e: DefaultEntry[K, V]) = if (super.findEntry(e.key) eq null) super.addEntry(e)
      sizeMapInit(table.length)
    }
    var i = 0
    while (i < ParHashMapCombiner.numblocks) {
      if (heads(i) ne null) {
        for (elem <- heads(i)) table.insertEntry(elem)
      }
      i += 1
    }
    // TODO compression
    val c = table.hashTableContents
    new ParHashMap(c)
  }
  
  /** A hash table which will never resize itself. Knowing the number of elements in advance,
   *  it allocates the table of the required size when created.
   *
   *  Entries are added using the `insertEntry` method. This method checks whether the element
   *  exists and updates the size map. It returns false if the key was already in the table,
   *  and true if the key was successfully inserted. It does not update the number of elements
   *  in the table.
   */
  private[ParHashMapCombiner] class AddingHashTable(numelems: Int, lf: Int) extends HashTable[K, DefaultEntry[K, V]] {
    import HashTable._
    _loadFactor = lf
    table = new Array[HashEntry[K, DefaultEntry[K, V]]](capacity(sizeForThreshold(_loadFactor, numelems)))
    tableSize = 0
    threshold = newThreshold(_loadFactor, table.length)
    sizeMapInit(table.length)
    def setSize(sz: Int) = tableSize = sz
    def insertEntry(/*block: Int, */e: DefaultEntry[K, V]) = {
      var h = index(elemHashCode(e.key))
      // assertCorrectBlock(h, block)
      var olde = table(h).asInstanceOf[DefaultEntry[K, V]]
      
      // check if key already exists
      var ce = olde
      while (ce ne null) {
        if (ce.key == e.key) {
          h = -1
          ce = null
        } else ce = ce.next
      }
      
      // if key does not already exist
      if (h != -1) {
        e.next = olde
        table(h) = e
        nnSizeMapAdd(h)
        true
      } else false
    }
    private def assertCorrectBlock(h: Int, block: Int) {
      val blocksize = table.length / (1 << ParHashMapCombiner.discriminantbits)
      if (!(h >= block * blocksize && h < (block + 1) * blocksize)) {
        println("trying to put " + h + " into block no.: " + block + ", range: [" + block * blocksize + ", " + (block + 1) * blocksize + ">")
        assert(h >= block * blocksize && h < (block + 1) * blocksize)
      }
    }
  }
  
  /* tasks */
  
  class FillBlocks(buckets: Array[Unrolled[DefaultEntry[K, V]]], table: AddingHashTable, offset: Int, howmany: Int)
  extends super.Task[Int, FillBlocks] {
    var result = Int.MinValue
    def leaf(prev: Option[Int]) = {
      var i = offset
      val until = offset + howmany
      result = 0
      while (i < until) {
        result += fillBlock(i, buckets(i))
        i += 1
      }
    }
    private def fillBlock(block: Int, elems: Unrolled[DefaultEntry[K, V]]) = {
      var insertcount = 0
      var unrolled = elems
      var i = 0
      val t = table
      while (unrolled ne null) {
        val chunkarr = unrolled.array
        val chunksz = unrolled.size
        while (i < chunksz) {
          val elem = chunkarr(i)
          // assertCorrectBlock(block, elem.key)
          if (t.insertEntry(elem)) insertcount += 1
          i += 1
        }
        i = 0
        unrolled = unrolled.next
      }
      insertcount
    }
    private def assertCorrectBlock(block: Int, k: K) {
      val hc = improve(elemHashCode(k))
      if ((hc >>> nonmasklen) != block) {
        println(hc + " goes to " + (hc >>> nonmasklen) + ", while expected block is " + block)
        assert((hc >>> nonmasklen) == block)
      }
    }
    def split = {
      val fp = howmany / 2
      List(new FillBlocks(buckets, table, offset, fp), new FillBlocks(buckets, table, offset + fp, howmany - fp))
    }
    override def merge(that: FillBlocks) {
      this.result += that.result
    }
    def shouldSplitFurther = howmany > collection.parallel.thresholdFromSize(ParHashMapCombiner.numblocks, parallelismLevel)
  }
  
}


private[mutable] object ParHashMapCombiner {
  private[mutable] val discriminantbits = 5
  private[mutable] val numblocks = 1 << discriminantbits
  private[mutable] val discriminantmask = ((1 << discriminantbits) - 1);
  private[mutable] val nonmasklength = 32 - discriminantbits
  
  def apply[K, V] = new ParHashMapCombiner[K, V](HashTable.defaultLoadFactor) with EnvironmentPassingCombiner[(K, V), ParHashMap[K, V]]
}














