package info.openmeta.framework.base.constant;

import java.util.Locale;

/**
 * Global base constant
 */
public interface BaseConstant {

    Locale DEFAULT_LANGUAGE = Locale.US;

    /** Debug parameter in request parameter */
    String DEBUG = "debug";
    /** The default top n value */
    Integer DEFAULT_TOP_N = 1;
    Integer DEFAULT_PAGE_NUMBER = 1;
    Integer DEFAULT_PAGE_SIZE = 50;
    Integer DEFAULT_BATCH_SIZE = 1000;
    Integer MAX_BATCH_SIZE = 10000;

    /** Cascading level restriction for cascade fields, for performance consideration, that is f0.f1.f2.f3.f4 */
    Integer CASCADE_LEVEL = 4;

    Integer DEFAULT_SCALE = 2;

    /** The optionSet code of Boolean field */
    String BOOLEAN_OPTION_CODE = "BooleanValue";

    /** The directory of predefined data, located in src/resources/data/ */
    String PREDEFINED_DATA_DIR = "data/";

    String SESSION_ID = "session_id";

    String TOKEN = "token";
    String AUTHORIZATION = "Authorization";
}
