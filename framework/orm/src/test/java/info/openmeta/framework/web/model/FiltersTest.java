package info.openmeta.framework.web.model;

import info.openmeta.framework.base.enums.Operator;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.orm.domain.Filters;
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
        String source = "[\"name\",\"=\",\"Test\"]";
        Filters filters = Filters.of(source);
        Assertions.assertEquals(source, filters.toString());
    }

    /**
     * Fault tolerance, redundancy []
     */
    @Test
    void parseUnitFilters() {
        String source = "[[\"name\",\"=\",\"Test\"]]";
        String expected = "[\"name\",\"=\",\"Test\"]";
        Filters filters = Filters.of(source);
        Assertions.assertEquals(expected, filters.toString());
    }

    /**
     * Fault tolerance, redundancy []
     */
    @Test
    void parseUnitFilters2() {
        String source = "[[\"name\",\"=\",\"Test\"],[]]";
        String expected = "[\"name\",\"=\",\"Test\"]";
        Filters filters = Filters.of(source);
        Assertions.assertEquals(expected, filters.toString());
    }

    /**
     * Default logic operator AND verification
     */
    @Test
    void parseDefaultLogicOperator() {
        String source = "[[\"name\",\"=\",\"Test\"],[\"active\",\"=\",false],[\"version\",\"=\",1]]";
        String expected = "[[\"name\",\"=\",\"Test\"],\"AND\",[\"active\",\"=\",false],\"AND\",[\"version\",\"=\",1]]";
        Filters filters = Filters.of(source);
        Assertions.assertEquals(expected, filters.toString());
    }

    /**
     * The case-insensitive compatibility of logical operators
     */
    @Test
    void parseLogicOperatorInsensitive() {
        String source = "[[\"name\",\"=\",\"Tom\"],\"and\",[\"active\",\"=\",false]]";
        String expected = "[[\"name\",\"=\",\"Tom\"],\"AND\",[\"active\",\"=\",false]]";
        Filters filters = Filters.of(source);
        Assertions.assertEquals(expected, filters.toString());
    }

    /**
     * Non-existent logical operator exception
     */
    @Test
    void parseLogicOperatorException() {
        String source = "[[\"name\",\"=\",\"Test\"],\"&&\",[\"active\",\"=\",false]]";
        Assertions.assertThrowsExactly(IllegalArgumentException.class, () -> Filters.of(source));
    }

    /**
     * Nested filters
     */
    @Test
    void parseNestedFilters() {
        String source = "[[[\"name\",\"=\",\"Tome\"],\"OR\",[\"name\",\"!=\",\"Demo\"]],\"AND\",[\"active\",\"=\",false]]";
        Filters filters = Filters.of(source);
        Assertions.assertEquals(source, filters.toString());
    }

    /**
     * Nested filters with default logic operator
     */
    @Test
    void parseNestedFiltersWithDefaultLogicOperator() {
        String source = "[[[[\"name\",\"!=\",\"A\"],[\"name\",\"!=\",\"B\"]],\"OR\",[\"code\",\"=\",\"Demo\"]],[\"active\",\"=\",false]]";
        String expected = "[[[[\"name\",\"!=\",\"A\"],\"AND\",[\"name\",\"!=\",\"B\"]],\"OR\",[\"code\",\"=\",\"Demo\"]],\"AND\",[\"active\",\"=\",false]]";
        Filters filters = Filters.of(source);
        Assertions.assertEquals(expected, filters.toString());
    }

    /**
     * Filters construction method operation verification
     */
    @Test
    void parseWithConstructionMethod() {
        String source = "[[\"name\",\"=\",\"Test\"],\"AND\",[\"active\",\"=\",false]]";
        Filters filters = Filters.of("name", Operator.EQUAL, "Test")
                .and(Filters.of("active", Operator.EQUAL, false));
        Assertions.assertEquals(source, filters.toString());
    }

    @Test
    void parseSemanticFormatSimple() {
        String filterString = "version = 1";
        Filters filters =  Filters.ofSemantic(filterString);
        assert filters != null;
        Assertions.assertEquals(filterString, filters.toSemanticString());

        filterString = "name = \"Test\" OR code = \"A01\"";
        filters =  Filters.ofSemantic(filterString);
        assert filters != null;
        Assertions.assertEquals(filterString, filters.toSemanticString());
    }

    @Test
    void parseSemanticFormatAnd() {
        String filterString = "name = \"Test\" AND code = \"A01\" AND version = 1";
        Filters filters =  Filters.ofSemantic(filterString);
        assert filters != null;
        Assertions.assertEquals(filterString, filters.toSemanticString());
    }

    @Test
    void parseSemanticFormatCombinedAnd() {
        String filterString = "(name = \"Test\" OR code = \"A01\") AND version = 1";
        Filters filters =  Filters.ofSemantic(filterString);
        assert filters != null;
        Assertions.assertEquals(filterString, filters.toSemanticString());
    }

    @Test
    void parseSemanticFormatCombinedOr() {
        String filterString = "name = \"Test\" OR (code = \"A01\" AND version = 1)";
        Filters filters =  Filters.ofSemantic(filterString);
        assert filters != null;
        Assertions.assertEquals(filterString, filters.toSemanticString());
    }

    @Test
    void parseSemanticFormatComplex() {
        String filterString = "((name = \"Tom\" AND code IN [\"A01\"]) OR version NOT IN [1]) AND version2 != 21";
        Filters filters =  Filters.ofSemantic(filterString);
        assert filters != null;
        log.info(filters.toString());
        log.info(filters.toSemanticString());
        Assertions.assertEquals(filterString, filters.toSemanticString());
    }
}