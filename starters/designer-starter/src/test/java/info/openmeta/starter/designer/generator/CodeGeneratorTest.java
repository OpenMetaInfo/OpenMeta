package info.openmeta.starter.designer.generator;

import info.openmeta.framework.orm.enums.FieldType;
import info.openmeta.framework.orm.utils.MapUtils;
import lombok.extern.slf4j.Slf4j;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

import static org.junit.jupiter.api.Assertions.assertNotNull;

@Slf4j
class CodeGeneratorTest {

    private Map<String, Object> mockModel() {
        return MapUtils.strObj()
                .put("userName", "Tom")
                .put("currentDate", "2024")
                .put("packageName", "info.openmeta.framework.demo")
                .put("modelName", "SysModel")
                .build();
    }

    private Map<String, Object> mockField() {
        FieldType[] fieldTypes = FieldType.values();
        int randomIndex = new Random().nextInt(fieldTypes.length);
        FieldType fieldType = fieldTypes[randomIndex];
        return MapUtils.strObj()
                .put("fieldType", fieldType.getType())
                .put("labelName", "fieldType: " + fieldType.getType())
                .put("fieldName", "DeptName")
                .put("relatedModel", "DeptInfo")
                .build();
    }

    @Test
    void generateService() {
        String code = CodeGenerator.generate("Service.ftl", mockModel());
        assertNotNull(code);
    }

    @Test
    void generateEntity() {
        Map<String, Object> modelData = mockModel();
        List<Map<String, Object>> modelFields = IntStream.range(0, 5).mapToObj(i -> mockField()).collect(Collectors.toList());
        modelData.put("modelFields", modelFields);
        String code = CodeGenerator.generate("Entity.ftl", modelData);
        assertNotNull(code);
    }
}