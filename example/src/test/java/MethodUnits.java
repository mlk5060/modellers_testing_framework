import info.peterlane.mdk.testing.*;

import org.junit.runners.Suite.SuiteClasses;
import org.junit.experimental.categories.Categories;
import org.junit.experimental.categories.Categories.IncludeCategory;
import org.junit.runner.RunWith;

@RunWith(Categories.class)
@IncludeCategory(UnitTest.class)
@SuiteClasses(MethodTests.class)
public class MethodUnits {
}
