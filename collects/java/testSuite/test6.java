public interface test6 {

    int getSize();

    Object lookup (int i)
    @pre{i < this.getSize();}
    @post{};

    void update(Object o, int i);

}
