/**
* huff_node
*
* This class is the storage object for elements of the Huffman tree.
*/

class huff_node
{
    // Instance variables.

    public float freq;          // Frequency.
    public int parent;          // Parent node.
    public int  left;           // Left pointer.
    public int right;           // Right pointer.
    public byte c;              // Byte value.

 // This class doesn't really need a constructor; we'll
 // just let the default object constructor do the job.

/**
* constructor
*/

huff_node()
{
    freq = (float) 0.0;
}

}
