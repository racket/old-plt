import java.awt.*;

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
