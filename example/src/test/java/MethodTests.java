
import info.peterlane.mdk.testing.*;

import static org.junit.Assert.assertEquals;
import org.junit.Test;
import org.junit.experimental.categories.Category;

public class MethodTests {

  @Category(UnitTest.class)
  @Test
  public void tryAddEm () {
    assertEquals (5, Methods.addEm (2, 3));
  }
  
  @Category(ProcessTest.class)
  @Test
  public void tryAddEm2 () {
    assertEquals (7, Methods.addEm (2, 5));
  }
  
  @Category(CanonicalResult.class)
  @Test
  public void tryAddEm3 () {
    assertEquals (10, Methods.addEm (2, 8));
  }

}
