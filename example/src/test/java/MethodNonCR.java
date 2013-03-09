import info.peterlane.mdk.*;

import org.junit.runners.Suite.SuiteClasses;
import org.junit.experimental.categories.Categories;
import org.junit.experimental.categories.Categories.ExcludeCategory;
import org.junit.runner.RunWith;

@RunWith(Categories.class)
@ExcludeCategory(CanonicalResult.class)
@SuiteClasses(MethodTests.class)
public class MethodNonCR {
}
