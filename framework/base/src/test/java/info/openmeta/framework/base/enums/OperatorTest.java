package info.openmeta.framework.base.enums;

import info.openmeta.framework.base.constant.BaseConstant;
import info.openmeta.framework.base.context.Context;
import info.openmeta.framework.base.context.ContextHolder;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;

class OperatorTest {

    /**
     * Create Context before all test cases
     */
    @BeforeAll
    static void initContext() {
        Context context = new Context();
        context.setLanguage(BaseConstant.DEFAULT_LANGUAGE);
        ContextHolder.setContext(context);
    }

    @Test()
    void ofEqual() {
        Operator operator = Operator.of("=");
        Assertions.assertEquals(Operator.EQUAL, operator);
    }

    @Test()
    void ofNotHas() {
        Operator operator = Operator.of("NOT HAS");
        Assertions.assertEquals(Operator.NOT_HAS, operator);
    }

    @Test()
    void ofBetween() {
        Operator operator = Operator.of("BETWEEN");
        Assertions.assertEquals(Operator.BETWEEN, operator);
    }

    @Test()
    void ofStartWith() {
        Operator operator = Operator.of("Start with");
        Assertions.assertEquals(Operator.START_WITH, operator);
    }

    @Test()
    void ofIsSet() {
        Operator operator = Operator.of("is set");
        Assertions.assertEquals(Operator.IS_SET, operator);
    }

    @Test()
    void ofParentOf() {
        Operator operator = Operator.of("PARENT OF");
        Assertions.assertEquals(Operator.PARENT_OF, operator);
    }

}