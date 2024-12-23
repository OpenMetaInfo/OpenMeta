package info.openmeta.framework.orm.domain;

import info.openmeta.framework.base.enums.Operator;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

@Slf4j
class FiltersTest {

    /**
     * Empty list
     */
    @Test
    void parseEmptyFilter() {
        String source = "[]";
        Filters filters = Filters.of(source);
        Assertions.assertEquals(source, filters.toString());
    }

    /**
     * FilterUnit exception
     */
    @Test
    void parseFilterUnitException() {
        String source = "[\"a\"]";
        Assertions.assertThrowsExactly(IllegalArgumentException.class, () -> Filters.of(source));
    }

    /**
     * FilterUnit exception
     */
    @Test
    void parseFilterUnitException2() {
        String source = "[\"a\",\"=\",1,2,3]";
        Assertions.assertThrowsExactly(IllegalArgumentException.class, () -> Filters.of(source));
    }

    /**
     * Operator exception
     */
    @Test
    void parseOperatorException() {
        String source = "[\"a\",\"&\"]";
        Assertions.assertThrowsExactly(IllegalArgumentException.class, () -> Filters.of(source));
    }

    /**
     * parse LEAF filters
     */
    @Test
    void parseFilterUnit() {
        String source = "[\"title\",\"=\",\"PM\"]";
        Filters filters = Filters.of(source);
        Assertions.assertEquals(source, filters.toString());
    }

    /**
     * Fault tolerance, redundancy []
     */
    @Test
    void parseUnitFilters() {
        String source = "[[\"title\",\"=\",\"PM\"]]";
        String expected = "[\"title\",\"=\",\"PM\"]";
        Filters filters = Filters.of(source);
        Assertions.assertEquals(expected, filters.toString());
    }

    /**
     * Fault tolerance, redundancy []
     */
    @Test
    void parseUnitFilters2() {
        String source = "[[\"title\",\"=\",\"PM\"],[]]";
        String expected = "[\"title\",\"=\",\"PM\"]";
        Filters filters = Filters.of(source);
        Assertions.assertEquals(expected, filters.toString());
    }

    /**
     * Default logic operator AND verification
     */
    @Test
    void parseDefaultLogicOperator() {
        String source = "[[\"title\",\"=\",\"PM\"],[\"active\",\"=\",false],[\"version\",\"=\",1]]";
        String expected = "[[\"title\",\"=\",\"PM\"],\"AND\",[\"active\",\"=\",false],\"AND\",[\"version\",\"=\",1]]";
        Filters filters = Filters.of(source);
        Assertions.assertEquals(expected, filters.toString());
    }

    /**
     * The case-insensitive compatibility of logical operators
     */
    @Test
    void parseLogicOperatorInsensitive() {
        String source = "[[\"title\",\"=\",\"PM\"],\"and\",[\"active\",\"=\",false]]";
        String expected = "[[\"title\",\"=\",\"PM\"],\"AND\",[\"active\",\"=\",false]]";
        Filters filters = Filters.of(source);
        Assertions.assertEquals(expected, filters.toString());
    }

    /**
     * Non-existent logical operator exception
     */
    @Test
    void parseLogicOperatorException() {
        String source = "[[\"title\",\"=\",\"PM\"],\"&&\",[\"active\",\"=\",false]]";
        Assertions.assertThrowsExactly(IllegalArgumentException.class, () -> Filters.of(source));
    }

    /**
     * Nested filters
     */
    @Test
    void parseNestedFilters() {
        String source = "[[[\"title\",\"=\",\"PM\"],\"OR\",[\"title\",\"!=\",\"Demo\"]],\"AND\",[\"active\",\"=\",false]]";
        Filters filters = Filters.of(source);
        Assertions.assertEquals(source, filters.toString());
    }

    /**
     * Nested filters with default logic operator
     */
    @Test
    void parseNestedFiltersWithDefaultLogicOperator() {
        String source = "[[[[\"title\",\"!=\",\"A\"],[\"title\",\"!=\",\"B\"]],\"OR\",[\"code\",\"=\",\"Demo\"]],[\"active\",\"=\",false]]";
        String expected = "[[[[\"title\",\"!=\",\"A\"],\"AND\",[\"title\",\"!=\",\"B\"]],\"OR\",[\"code\",\"=\",\"Demo\"]],\"AND\",[\"active\",\"=\",false]]";
        Filters filters = Filters.of(source);
        Assertions.assertEquals(expected, filters.toString());
    }

    /**
     * Filters construction method operation verification
     */
    @Test
    void parseWithConstructionMethod() {
        String source = "[[\"title\",\"=\",\"PM\"],\"AND\",[\"active\",\"=\",false]]";
        Filters filters = Filters.of("title", Operator.EQUAL, "PM")
                .and(Filters.of("active", Operator.EQUAL, false));
        Assertions.assertEquals(source, filters.toString());
    }

    @Test
    void parseSemanticFormatSimple() {
        String filterString = "version = 1";
        Filters filters =  Filters.ofSemantic(filterString);
        assert filters != null;
        Assertions.assertEquals(filterString, filters.toSemanticString());

        filterString = "title = \"PM\" OR code = \"A01\"";
        filters =  Filters.ofSemantic(filterString);
        assert filters != null;
        Assertions.assertEquals(filterString, filters.toSemanticString());
    }

    @Test
    void parseSemanticFormatAnd() {
        String filterString = "title = \"PM\" AND code = \"A01\" AND version = 1";
        Filters filters =  Filters.ofSemantic(filterString);
        assert filters != null;
        Assertions.assertEquals(filterString, filters.toSemanticString());
    }

    @Test
    void parseSemanticFormatCombinedAnd() {
        String filterString = "(title = \"PM\" OR code = \"A01\") AND version = 1";
        Filters filters =  Filters.ofSemantic(filterString);
        assert filters != null;
        Assertions.assertEquals(filterString, filters.toSemanticString());
    }

    @Test
    void parseSemanticFormatCombinedOr() {
        String filterString = "title = \"PM\" OR (code = \"A01\" AND version = 1)";
        Filters filters =  Filters.ofSemantic(filterString);
        assert filters != null;
        Assertions.assertEquals(filterString, filters.toSemanticString());
    }

    @Test
    void parseSemanticFormatComplex() {
        String filterString = "((title = \"PM\" AND code IN [\"A01\"]) OR version NOT IN [1]) AND version2 != 21";
        Filters filters =  Filters.ofSemantic(filterString);
        assert filters != null;
        log.info(filters.toString());
        log.info(filters.toSemanticString());
        Assertions.assertEquals(filterString, filters.toSemanticString());
    }

    /**
     * Convert entity to Filters object with non-null values
     */
    @Test
    void ofEntityWithNonNullValues() {
        TestJobEntity entity = new TestJobEntity("PM", 1);
        Filters filters = Filters.ofEntity(entity, false);
        String expected = "[[\"title\",\"=\",\"PM\"],\"AND\",[\"value\",\"=\",1]]";
        Assertions.assertEquals(expected, filters.toString());
    }

    /**
     * Convert entity to Filters object with null values ignored
     */
    @Test
    void ofEntityWithNullValuesIgnored() {
        TestJobEntity entity = new TestJobEntity(null, 1);
        Filters filters = Filters.ofEntity(entity, true);
        String expected = "[\"value\",\"=\",1]";
        Assertions.assertEquals(expected, filters.toString());
    }

    /**
     * Convert entity to Filters object with null values not ignored
     */
    @Test
    void ofEntityWithNullValuesNotIgnored() {
        TestJobEntity entity = new TestJobEntity(null, 1);
        Filters filters = Filters.ofEntity(entity, false);
        String expected = "[[\"title\",\"IS NOT SET\",null],\"AND\",[\"value\",\"=\",1]]";
        Assertions.assertEquals(expected, filters.toString());
    }

    /**
     * Convert empty entity to Filters object
     */
    @Test
    void ofEntityWithEmptyEntity() {
        TestJobEntity entity = new TestJobEntity(null, null);
        Filters filters = Filters.ofEntity(entity, true);
        String expected = "[]";
        Assertions.assertEquals(expected, filters.toString());
    }

    /**
     * Convert entity to Filters object with all null values
     */
    @Test
    void ofEntityWithAllNullValues() {
        TestJobEntity entity = new TestJobEntity(null, null);
        Filters filters = Filters.ofEntity(entity, false);
        String expected = "[[\"title\",\"IS NOT SET\",null],\"AND\",[\"value\",\"IS NOT SET\",null]]";
        Assertions.assertEquals(expected, filters.toString());
    }

    @Test
    void and1() {
        Filters filters1 = Filters.of("title", Operator.EQUAL, "PM")
                .or(Filters.of("active", Operator.EQUAL, false));
        Filters filters2 = Filters.of("version", Operator.EQUAL, 1);
        Filters filters = filters1.and(filters2);
        String expected = "[[[\"title\",\"=\",\"PM\"],\"OR\",[\"active\",\"=\",false]],\"AND\",[\"version\",\"=\",1]]";
        Assertions.assertEquals(expected, filters.toString());
    }

    @Test
    void and2() {
        Filters filters1 = new Filters().eq("title", "PM")
                .or(new Filters().eq("active", false));
        Filters filters2 = new Filters().eq("version", 1);
        Filters filters = filters1.and(filters2);
        String expected = "[[[\"title\",\"=\",\"PM\"],\"OR\",[\"active\",\"=\",false]],\"AND\",[\"version\",\"=\",1]]";
        Assertions.assertEquals(expected, filters.toString());
    }

    @Test
    void and3() {
        Filters filters1 = Filters.and().eq("title", "PM")
                .or("active", Operator.EQUAL, Boolean.FALSE);
        Filters filters2 = Filters.of("version", Operator.EQUAL, 1);
        Filters filters = filters1.and(filters2);
        String expected = "[[[\"title\",\"=\",\"PM\"],\"OR\",[\"active\",\"=\",false]],\"AND\",[\"version\",\"=\",1]]";
        Assertions.assertEquals(expected, filters.toString());
    }
}