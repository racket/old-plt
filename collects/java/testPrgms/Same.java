/*

Same
Copyright (c) 1998, Robby Findler
http://www.cs.rice.edu/~robby/

This program fully redistributable, as long as the above copyright
remains intact.

*/

import java.lang.Character.*;
import java.lang.Boolean.*;
import java.awt.event.*;
import java.awt.*;
import java.applet.Applet;
import java.io.*;

interface Predicate {
    boolean compare(Object n1,Object n2);
}

interface Function {
    Object f(Object n,Object o);
}

interface Iterator {
    void f(Object n);
}

interface List {
    List Sort (Predicate p);
    List Insert (Predicate p, Object n);
    int Length();
    boolean NotNullp();
    boolean Nullp();
    Object Foldr(Function f, Object o);
    void ForEach(Iterator f);
    public List Reverse();
    public List ReverseHelp(List m);
}  

class Node {
    Color color;
    boolean visited = false;
    int i;
    int j;

    public Node(java.awt.Color _color, int _i, int _j) {
	color=_color; 
	i=_i;
	j=_j;
    }
    
    public String toString() {
	return new String("(" + i + " " + j + " " + color + ")");
    }

    public void set_visit(boolean _visited) {
	visited = _visited;
    }
    public void set_color(Color _color) {
	color = _color;
    }
    
    public void set_ij(int _i, int _j) {
	i=_i;
	j=_j;
    }
}

class Null implements List {
    static public List NULL = new Null();

    public Null() {}
    public int Length() {return 0;}
    public List Sort(Predicate p) {return this;}
    public List Insert(Predicate p,Object item) {return new Cons(item,this);}
    public boolean Nullp() {return true;}
    public boolean NotNullp() {return false;}
    public String toString() {return new String("");}
    public Object Foldr(Function f, Object o) {return o;}
    public void ForEach(Iterator f) {}
    public List Reverse() {return this;}
    public List ReverseHelp(List m) {return m;}
}

class Cons implements List {
  
    Object hd;
    List tl;
    
    public Cons(Object _hd, List _tl) {
	hd=_hd;
	tl=_tl;
    }
    
    public int Length () { return 1 + tl.Length(); }
    
    public List Sort(Predicate p) { return tl.Sort(p).Insert(p,hd); }
    
    public List Insert(Predicate p, Object n) {
	if (p.compare(hd,n)) {
	    return new Cons(hd, tl.Insert(p,n));
	} else {
	    return new Cons (n,this);
	}
    }
    public boolean NotNullp() {return true;}
    public boolean Nullp() {return false;}
    public String toString() {return new String(hd.toString()+ tl.toString());}
    public void ForEach(Iterator f) {
	f.f(hd);
	tl.ForEach(f);
    }
    public Object Foldr(Function f, Object o) { return tl.Foldr(f,f.f(hd,o)); }
    public List ReverseHelp(List m) { return tl.ReverseHelp(new Cons (hd,m));}
    public List Reverse() {return ReverseHelp(Null.NULL);}
}

class SortJ implements Predicate {
    public boolean compare(Object n1,Object n2) {
	return ((Node)n1).j < ((Node)n2).j;
    }
}

interface BoardListener {
    void EraseGameOver();
    void ColumnChanged(int i);
    void GameOver();
}

interface ScoreListener {
    void ScoreChanged(int new_score);
}

class Move {
    int score;
    int i;
    int j;

    public Move(int _score, int _i, int _j) {
	i=_i;
	j=_j;
	score=_score;
    }
}

class SameGame {
    int score = 0;
    static int board_width = 20;
    static int board_height = 10;
    boolean game_over = false;
    
    Node[][] board = null;

    List moves = Null.NULL;

    void initialize_board(long seed) {
	int num_colors = 5;
	java.awt.Color colors[] = new java.awt.Color[num_colors];
	colors[0] = java.awt.Color.blue;
	colors[1] = java.awt.Color.red;
	colors[2] = java.awt.Color.magenta;
	colors[3] = java.awt.Color.yellow;
	colors[4] = java.awt.Color.cyan;

	java.util.Random r = new java.util.Random(seed);
    
	board = new Node[board_width][board_height];

	for (int i=0; i < board_width; i++) {
	    for (int j=0; j < board_height; j++) {
		int ci = (int)(r.nextFloat() * num_colors);
		if (ci == 5) { ci = 0; }
		/*
		  if (j == board_height-1) {
		  board[i][j]=new Node (colors[num_colors-1],i,j);
		  } else {
		  board[i][j]=new Node (colors[i % (num_colors-1)],i,j);  
		  }
		*/
		//board[i][j]=new Node(colors[i % num_colors],i,j);
		//board[i][j]=new Node(colors[0],i,j);
		board[i][j]=new Node(colors[ci],i,j);
	    }
	}
    }

    public void Restart(long seed) {
	SetScore(0);
	game_over=false;
	moves = Null.NULL;

	initialize_board(seed);
	board_listeners.ForEach(new BoardListenersEraseGameOver());
	for (int i = 0; i < board_width; i++) {
	    board_listeners.ForEach(new BoardListenersRedrawColumn(i));
	}
    }

    public SameGame (long seed) {
	initialize_board(seed);
    }

    private List find_some_colors_help (Color looking, int i, int j, List cells) {
    	if (0 <= i && i < board_width && 0 <= j && j < board_height) {
	    Node n = board[i][j];
	    if (! n.visited && n.color == looking) {
		n.set_visit(true);
		return find_some_colors_help
		    (looking, i-1,j,
		     find_some_colors_help
		     (looking, i+1,j,
		      find_some_colors_help
		      (looking, i,j-1,
		       find_some_colors_help
		       (looking, i,j+1,
			new Cons(n, cells)))));
	    } else {
		return cells;
	    }
    	} else {
	    return cells;
	}
    }

    public List find_some_colors (int i, int j) {
	Node n = board[i][j];

	if (n.color != java.awt.Color.white) {
	    List cells = find_some_colors_help(n.color, i, j, Null.NULL);
      
	    class ResetVisit implements Iterator {
		public void f (Object n) {
		    ((Node)n).set_visit(false);
		    return ;
		}
	    }
	    cells.ForEach(new ResetVisit());
	    return cells;
	}
	return Null.NULL;
    }

    class CallScoreListeners implements Iterator {
	int new_score;
	
	public CallScoreListeners(int _new_score) {
	    new_score=_new_score;
	}

	public void f (Object sl) {
	    ((ScoreListener)sl).ScoreChanged(new_score);
	}
    }

    List score_listeners = Null.NULL;
    public void AddScoreListener(ScoreListener s) {
	score_listeners=new Cons(s,score_listeners);
    }
    public void SetScore(int new_score) {
	score=new_score;
	score_listeners.ForEach(new CallScoreListeners(new_score));
    }

    public int calc_score(int n) {
	if (n == 2) {
	    return 2;
	} else {
	    return (n-1)*(n-1) - (n-3);
	}
    }

    List board_listeners=Null.NULL;
    public void AddBoardListener(BoardListener b) {
	board_listeners = new Cons (b,board_listeners);
    }

    class BoardListenersEraseGameOver implements Iterator {
	public void f(Object board_listener) {
	    ((BoardListener)board_listener).EraseGameOver();
	}
    }

    class BoardListenersRedrawColumn implements Iterator {
	int column;
	
	public BoardListenersRedrawColumn (int _column) {
	    column=_column;
	}
	public void f(Object board_listener) {
	    ((BoardListener)board_listener).ColumnChanged(column);
	}
    }

    class BoardListenersGameOver implements Iterator {
	public void f(Object board_listener) {
	    ((BoardListener)board_listener).GameOver();
	}
    }

    // returns true if something changed; false otherwise
    public boolean RemoveAt (int i, int j) {
	List cells = find_some_colors (i,j);
	int l;  // loop variable
	int m;  // loop variable

	
	// if the user found something to delete
	if (cells.Nullp() || ((Cons)cells).tl.Nullp()) {
	    return false;
	}
            
	SetScore(score + calc_score(cells.Length()));

	// update the moves list with current move
	moves = new Cons(new Move (score,i,j),moves);

	final Boolean changed[] = new Boolean[board_width];

	// slide rows down

	class SlideDown implements Iterator {
	    public void f (Object _n) {
		Node n = ((Node)_n);

		changed[n.i] = java.lang.Boolean.TRUE;
              
		for (int q = n.j; q > 0 ; q--) {
		    board[n.i][q].set_color(board[n.i][q-1].color);
		}
		board[n.i][0].set_color(java.awt.Color.white);
	    }
	}

	cells.Sort(new SortJ()).ForEach(new SlideDown());

	// slide columns over
	for (m=board_width-1 ; m >= 0 ; m--) {
	    if (board[m][board_height-1].color == java.awt.Color.white) {
		for (l = m; l <= board_width-2; l++) {
		    changed[l] = java.lang.Boolean.TRUE;
		    board[l]=board[l+1];
		}
		changed[board_width-1]=java.lang.Boolean.TRUE;
		board[board_width-1]=new Node[board_height];
		for (l=0; l<board_height; l++) {
		    board[board_width-1][l]=
			new Node (java.awt.Color.white,board_width-1,l);
		}
	    }
	}
	    
	// clean up the coordinates
	for (l = 0; l < board_width; l++) {
	    for (m = 0; m < board_height; m++) {
		board[l][m].set_ij(l,m);
	    }
	}

	for (l=0; l < board_width ; l++) {
	    if (changed[l] != null) {
		board_listeners.ForEach(new BoardListenersRedrawColumn(l));
	    }
	}

	game_over = true;
	for (l=0; l < board_width; l++) {
	    for (m=0; m < board_height; m++) {
		List colors = find_some_colors(l,m);
		game_over = game_over && (colors.Nullp() || ((Cons)colors).tl.Nullp());
	    }
	}
	if (game_over) {
	    board_listeners.ForEach(new BoardListenersGameOver());
	}
	return true;
    }
}

class SameCanvas extends Canvas implements BoardListener, ScoreListener {
    int board_width = SameGame.board_width;
    int board_height = SameGame.board_height;

    int cell_size = 25;
    List old_cells = Null.NULL;
    private SameGame same_game;

    Label this_score_label;
    Label score_label;
    public void set_labels (Label _score_label, Label _this_score_label) {
	this_score_label = _this_score_label;
	score_label = _score_label;
    }

    public boolean mouseUp(Event evt, int x, int y) {
	int i = x / cell_size;
	int j = y / cell_size;

	UpdateSmallDots(x,y);
	if (0 <= i && i < board_width && 0 <= j && j < board_height) {
	    if (same_game.RemoveAt (i,j)) {
		old_cells=Null.NULL;
		UpdateSmallDots(x,y);
	    }
	}
	return true;
    }

    public boolean mouseExit(Event evt, int x, int y) {
	old_cells.ForEach(new DrawCell());
	this_score_label.setText("");
	old_cells=Null.NULL;
	return true;
    }

    public boolean mouseMove (Event evt, int x, int y) {
	UpdateSmallDots(x,y);
	return true;
    }

    public void ScoreChanged(int new_score) {
	score_label.setText(new Integer(new_score).toString());
    }

    public SameCanvas(SameGame _same_game) {
	same_game=_same_game;
	same_game.AddBoardListener(this);
	same_game.AddScoreListener(this);
    }

    public void draw_cell(Graphics g, int i, int j) {
	g.setColor(same_game.board[i][j].color);
	g.fillOval(i*cell_size, j*cell_size, cell_size, cell_size);
    }

    public void draw_dot_cell(Graphics g, int i, int j) {
	g.setColor(java.awt.Color.white);
	g.fillOval(i*cell_size, j*cell_size, cell_size, cell_size);
	g.setColor(same_game.board[i][j].color);
	g.fillOval(i*cell_size + cell_size/4, j*cell_size + cell_size/4,
		   cell_size/2, cell_size/2);
    }
   
    public void EraseGameOver() {
	draw_game_over(getGraphics(),false);
    }

    public void ColumnChanged (int i) {
	draw_line(getGraphics(),i);
    }

    public void draw_line (Graphics g, int i) {
	for (int j = 0; j < board_height; j++) {
	    draw_cell(g, i, j);
	}
    }

    public void GameOver() {
	draw_game_over(getGraphics(),true);
    }

    void draw_game_over(Graphics g, boolean is_game_over) {
	Font f = new Font("SansSerif",java.awt.Font.PLAIN,36);

	g.setFont(f);

	String game_over = new String ("Game Over");
	FontMetrics fm = g.getFontMetrics();
	int sw = fm.stringWidth(game_over);
	int sh = fm.getAscent() + fm.getDescent() + fm.getLeading();
	int xc = 250;
	int yc = 125;
	int border = sh/4;
	int x_pos = xc - (sw/2);
	int y_pos = yc - (sh/2);
	g.setColor(java.awt.Color.white);
	g.fillRect(x_pos - border,y_pos - border, sw + border * 2, sh + border * 2);

	if (is_game_over) {
	    g.setColor(java.awt.Color.black);
	    g.drawString(game_over,x_pos, y_pos + fm.getLeading() + fm.getAscent());
	}
    }

    public void paint(Graphics g) {
	for (int i = 0; i < board_width; i++) {
	    draw_line (g,i);
	}
	if (same_game.game_over) {
	    draw_game_over(g,true);
	}
    }

    class DrawDotCell implements Iterator {
	public void f(Object _n) {
	    Node n = ((Node)_n);
	    draw_dot_cell(getGraphics(), n.i, n.j);
	}
    }
    class DrawCell implements Iterator {
	public void f(Object _n) {
	    Node n = ((Node)_n);
	    draw_cell(getGraphics(), n.i, n.j);
	}
    }
    
    void UpdateSmallDots (int x, int y) {
     	final int i = x / cell_size;
	final int j = y / cell_size;

	int l;  // loop variable
	int m;  // loop variable

        if (0 <= i && i < board_width && 0 <= j && j < board_height) {

	    class CheckAlreadyIn implements Function {
		public Object f(Object _n, Object _sofar) {
		    Node n = ((Node)_n);
		    boolean sofar = ((Boolean)_sofar).booleanValue();
		    return new Boolean(sofar || (n.i == i && n.j == j));
		}
	    }
	    boolean already_in = 
		((Boolean)old_cells.Foldr(new CheckAlreadyIn(), 
					  java.lang.Boolean.FALSE)).booleanValue();
          
	    if (!already_in) {
		List cells = same_game.find_some_colors(i,j);	      
		old_cells.ForEach(new DrawCell());
		if (cells.NotNullp() && ((Cons)cells).tl.NotNullp()) {
		    old_cells = cells;
		    old_cells.ForEach(new DrawDotCell());
		    this_score_label.setText((new Integer(same_game.calc_score(cells.Length()))).toString());
		} else {
		    this_score_label.setText("");
		    old_cells = Null.NULL;
		}
	    }
	} 
    }
}

class PlayBoard implements Iterator {
    SameGame s;

    public PlayBoard(SameGame _s) { s=_s; };

    public void f (Object _m) {
	Move m = (Move)_m;
	//try {java.lang.Thread.sleep(800);}
	//catch (InterruptedException e) {}
	s.RemoveAt(m.i, m.j);
    }
}

class NewGameButton extends Button {
  SameGame s;

  public NewGameButton(String _n, SameGame _s) {
    super(_n);
    s=_s;
  }
  public boolean action(Event evt, Object what) {
    s.Restart(System.currentTimeMillis());
    return true;
  }
}


public class Same extends Applet {
    public void start() {
	;
    }

    public void stop() {
	;
    }

    public void destroy() {
	;
    }

    public void init () {
	//final SameGame s = new SameGame(java.lang.System.currentTimeMillis());
	SameGame s = new SameGame(java.lang.System.currentTimeMillis());
	SameCanvas c = new SameCanvas(s);

	Panel q = new Panel();

	Panel p = new Panel();
	int p_height = 50;
	Panel r = new Panel();
	int r_height = 50;

	Label l1 = new Label("Score: ");
	Label l2 = new Label("0");
	Label l3 = new Label("This Click: ");
	Label l4 = new Label("");

	Button sb = new Button("Submit High Score");
	Button nb = new NewGameButton("New Game",s);

	add(c);
	c.resize(500,250);

	q.setLayout(new GridLayout(0,1));
	add(q);

	p.setLayout(new GridLayout(0,4));
	q.add(p);
	p.add(l1);
	p.add(l2);
	p.add(l3);
	p.add(l4);

	r.setLayout(new GridLayout(0,1));
	q.add(r);
	//r.add(sb);
	r.add(nb);
	
	validate();
	p.resize(500,l1.size().height);
	r.resize(500,nb.size().height);
	//p.resize(500,l1.getSize().height);
	//r.resize(500,nb.getSize().height);
	validate();

	setBackground(java.awt.Color.white);
	c.set_labels(l2,l4);

	//NewGameButtonListener blah = new NewGameButtonListener(s); 
	//nb.addActionListener(blah);
    }

    public static void main(String args[]) {
	Applet a = new Same();

	Frame f = new Frame("Same");
	f.add("Center", a);
	f.resize(550,400);
	f.show();
	a.init();
	a.start();
    }
}

class NewGameButtonListener implements ActionListener {
  SameGame s;

  public NewGameButtonListener(SameGame _s) { s = _s; }
  public void actionPerformed(ActionEvent evt) {
    s.Restart(System.currentTimeMillis());
  }
}

class Binding {
    String lhs;
    String rhs;

    public Binding(String _lhs, String _rhs) {
	lhs=_lhs;
	rhs=_rhs;
    }
}

// This relies on a platform-specific newline convention
// for parsing the cgi web server output.
// Why doesn't java have a text-mode for reading from streams?
class SameServer {

    char read_char (FileReader fr) throws IOException {
	char[] cbuf=new char[1];
	if (fr.read(cbuf,0,1) == -1) {
	    throw new IOException();
	} else {
	    return cbuf[0];
	}
    }

    List read_until(FileReader fr, char sep) {
	boolean done = false;
	List l = Null.NULL;

	for (;!done;) {
	    try {
		char c=read_char(fr);
		if (c == sep || c == '\n' || c == '\r') {
		    done = true;
		} else if (c == '+') {
		    Character c1=null;
		    Character c2=null;
		    try {
			c1=new Character(read_char(fr));
			c2=new Character(read_char(fr));
			byte b[] = new byte[1];
			
			if (c1.charValue() != 'e' ||
			    c2.charValue() != 'e') {
			    
			    throw new IOException();
			}
			l = new Cons (new Character(' '),l);
		    } catch (IOException e) {
			System.out.println("Badly formatted %; got " + c1 + " and " + c2);
			done = true;
		    }
		} else if (c == '%') {
		    Character c1=null;
		    Character c2=null;
		    try {
			c1=new Character(read_char(fr));
			c2=new Character(read_char(fr));
			byte b[] = new byte[1];
			
			if (!Character.isDigit(c1.charValue()) ||
			    !Character.isDigit(c2.charValue())) {
			    
			    throw new IOException();
			}
			b[0] = (byte)(Character.digit(c1.charValue(),16)*16 +
				      Character.digit(c2.charValue(),16));
			l = new Cons (new Character(new String(b).charAt(0)),l);
		    } catch (IOException e) {
			System.out.println("Badly formatted %; got " + c1 + " and " + c2);
			done = true;
		    }
		} else {
		    l = new Cons (new Character(c),l);
		}
	    } catch (IOException e) {
		done=true;
	    }
	}
	return l.Reverse();
    }

    List bindings = Null.NULL;

    void build_bindings(FileReader fr) {
	boolean done = false;

	for (;!done;) {
	    List l1 = read_until(fr,'=');
	    List l2 = read_until(fr,'\n');

	    if (l1.Nullp()) {
		done=true;
	    } else {
		bindings=new Cons(new Binding(l1.toString(),l2.toString()),bindings);
	    }
	}
    }

    public String fetch_string(final String binder) {
	class find_string implements Iterator {
	    String answer = "<empty>";
	
	    public void f (Object o) {
		Binding b = (Binding)o;
		if (b.lhs.equalsIgnoreCase(binder)) {
		    answer = b.rhs;
		}
	    }
	}

	find_string f = new find_string();
	bindings.ForEach(f);
	return f.answer;
    }

    public int fetch_int(String binder) {
	String s = fetch_string(binder);
	return Integer.parseInt(s);
    }

    public long fetch_long(String binder) {
	String s = fetch_string(binder);
	return Long.parseLong(s);
    }

    public List fetch_moves() {
	int count = fetch_int("MOVE_COUNT");
	return Null.NULL;
    }

    public SameServer() throws FileNotFoundException {
	/*
	if (!java.lang.System.getenv("REQUEST_METHOD").equals(new String ("POST"))) {
	    System.out.println ("<html>Error -- not post method </html>");
	    return;
	}
	*/
	build_bindings(new FileReader ("sample-input.txt"));
	int score = fetch_int("SCORE");
	System.out.println("score " + score);

	long seed = fetch_long("SEED");
	System.out.println("seed " + seed);

	List l = fetch_moves();
	SameGame s = new SameGame(seed);
	l.ForEach(new PlayBoard(s));
	if (s.score == score) {
	    System.out.println("passed");
	} else {
	    System.out.println("match failed");
	}
	System.out.println("SameServer");

    }

    static public void main (String[] argv) throws FileNotFoundException {
	new SameServer();
    }
}

