package info.openmeta.framework.orm.utils;

import info.openmeta.framework.orm.meta.MetaModel;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

class LambdaUtilsTest {

    @Test
    void getAttributeName() {
        String attribute = LambdaUtils.getAttributeName(MetaModel::getLabelName);
        Assertions.assertEquals("labelName", attribute);
    }

}