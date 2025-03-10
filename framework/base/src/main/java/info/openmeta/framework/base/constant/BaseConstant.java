package info.openmeta.framework.base.constant;

import info.openmeta.framework.base.enums.Language;

/**
 * Global base constant
 */
public interface BaseConstant {

    Language DEFAULT_LANGUAGE = Language.EN_US;

    /** Debug parameter in request parameter */
    String DEBUG = "debug";
    /** The default top n value */
    Integer DEFAULT_TOP_N = 1;
    Integer DEFAULT_PAGE_NUMBER = 1;
    Integer DEFAULT_PAGE_SIZE = 50;
    Integer DEFAULT_BATCH_SIZE = 1000;
    Integer MAX_BATCH_SIZE = 10000;
    Integer MAX_EXPORT_SIZE = 100000;
    Integer DEFAULT_NAME_LIST_SIZE = 10;

    /** Cascading level restriction for cascade fields, for performance consideration, that is f0.f1.f2.f3.f4 */
    Integer CASCADE_LEVEL = 4;

    Integer DEFAULT_SCALE = 2;

    /** The optionSet code of Boolean field */
    String BOOLEAN_OPTION_SET_CODE = "BooleanValue";

    /** The directory of predefined data, located in src/resources/data/ */
    String PREDEFINED_DATA_DIR = "data/";

    String SESSION_ID = "session_id";

    String TOKEN = "token";
    String AUTHORIZATION = "Authorization";

    // TraceId in request header
    String TRACE_ID = "traceId";
    String X_TRACE_ID = "X-Trace-Id";
    String X_B3_TRACEID = "X-B3-TraceId";
}
