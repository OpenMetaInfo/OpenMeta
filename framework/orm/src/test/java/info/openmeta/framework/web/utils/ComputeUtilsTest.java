package info.openmeta.framework.web.utils;

import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.runtime.JavaMethodReflectionFunctionMissing;
import info.openmeta.framework.orm.compute.ComputeUtils;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.*;

@Slf4j
class ComputeUtilsTest {

    @Test
    void reflectMethod() {
        AviatorEvaluatorInstance instance = AviatorEvaluator.newInstance();
        instance.setFunctionMissing(JavaMethodReflectionFunctionMissing.getInstance());

        String formula =  "a='abc';length(a)";
        Object result = instance.execute(formula);
        Assertions.assertEquals("3", result.toString());

        formula = "getYear(date)";
        Map<String, Object> env = new HashMap<>();
        LocalDate date = LocalDate.now();
        env.put("date", date);
        result = instance.compile(formula).execute(env);
        Assertions.assertEquals(String.valueOf(date.getYear()), result.toString());
    }

    /**
     * Import LocalDate static methods and instance methods
     */
    @Test
    void getNowDate() {
        String formula = "LocalDate.now()";
        Object result = ComputeUtils.execute(formula);
        Assertions.assertEquals(LocalDate.now(), result);
    }

    /**
     * Import LocalDate static methods and instance methods
     */
    @Test
    void parseStringDate() {
        String formula = "LocalDate.parse('2022-11-11')";
        Object result = ComputeUtils.execute(formula);
        Assertions.assertEquals(LocalDate.parse("2022-11-11"), result);
    }

    /**
     * Import LocalDate, DateTimeFormatter static methods and instance methods.
     * Does not support chaining calls DateTimeFormatter.ofPattern('yyyy-MM').format(date).
     */
    @Test
    void convertDate() {
        // String formula = "a = DateTimeFormatter.ofPattern('yyyy-MM');DateTimeFormatter.format(a, date)";
        String formula = "LocalDate.format(date, DateTimeFormatter.ofPattern('yyyy-MM'))";
        LocalDate date = LocalDate.now();
        Map<String, Object> env = new HashMap<>();
        env.put("date", date);
        Object result = ComputeUtils.compile(formula).execute(env);
        Assertions.assertEquals(DateTimeFormatter.ofPattern("yyyy-MM").format(date), result);
    }

    /**
     * Import LocalDate static methods and instance methods
     */
    @Test
    void computeDateYear() {
        String formula = "LocalDate.plusYears(date, 5)";
        LocalDate date = LocalDate.now();
        Map<String, Object> env = new HashMap<>();
        env.put("date", date);
        Object result = ComputeUtils.compile(formula).execute(env);
        Assertions.assertEquals(date.plusYears(5), result);
    }

    /**
     * Reflect call LocalDate object method
     */
    @Test
    void reflectComputeDateYear() {
        AviatorEvaluatorInstance instance = AviatorEvaluator.newInstance();
        instance.setFunctionMissing(JavaMethodReflectionFunctionMissing.getInstance());

        String formula =  "plusYears(date, 5)";
        LocalDate date = LocalDate.now();
        Map<String, Object> env = new HashMap<>();
        env.put("date", date);
        Object result = instance.compile(formula).execute(env);
        Assertions.assertEquals(date.plusYears(5), result);
    }

    @Test
    void getFromList() {
        String formula = "data[0]";
        List<String> data = new ArrayList<>(Arrays.asList("a", "b", "c"));
        Map<String, Object> env = new HashMap<>();
        env.put("data", data);
        Object result = ComputeUtils.compile(formula).execute(env);
        Assertions.assertEquals(data.getFirst(), result);
    }

    @Test
    void getFromMap() {
        String formula = "data.a";
        Map<String, Object> data = Map.of("a", "Tab", "c", 2);
        Map<String, Object> env = new HashMap<>();
        env.put("data", data);
        Object result = ComputeUtils.compile(formula).execute(env);
        Assertions.assertEquals(data.get("a"), result);
    }

    @Test
    void getFromMap2() {
        String formula = "data.a.b";
        String b = "Max";
        Map<String, Object> data = Map.of("a", Map.of("b", b), "c", 2);
        Map<String, Object> env = new HashMap<>();
        env.put("data", data);
        Object result = ComputeUtils.compile(formula).execute(env);
        Assertions.assertEquals(b, result);
    }

    @Test
    void decimal() {
        String formula = " 1 / 3 * 3";
        Object result1 = ComputeUtils.compile(formula).execute();
        BigDecimal result2 = new BigDecimal("1").divide(new BigDecimal("3"), 16, RoundingMode.HALF_EVEN).multiply(new BigDecimal("3"));
        Assertions.assertEquals(result1, result2);
    }

}