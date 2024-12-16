package ${packageName}.entity;

import info.openmeta.framework.orm.entity.BaseModel;
import com.fasterxml.jackson.databind.JsonNode;
import lombok.Data;
import io.swagger.v3.oas.annotations.media.Schema;
import java.io.Serializable;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.io.Serial;
import java.util.List;

/**
* ${modelName} model entity
* @author ${userName}
* @date ${currentDate}
*/
@Data
@Schema(name = "${modelName}")
public class ${modelName} extends BaseModel {

    @Serial
    private static final long serialVersionUID = 1L;

<#list modelFields as field>
<#assign javaType = field.fieldType>
<#switch field.fieldType>
    <#case "Date"><#assign javaType = "LocalDate"><#break>
    <#case "DateTime"><#assign javaType = "LocalDateTime"><#break>
    <#case "Option"><#assign javaType = "String"><#break>
    <#case "MultiString">
    <#case "MultiOption"><#assign javaType = "List<String>"><#break>
    <#case "JSON"><#assign javaType = "JsonNode"><#break>
    <#case "Filter"><#assign javaType = "Filters"><#break>
    <#case "OneToOne">
    <#case "ManyToOne"><#assign javaType = "Long"><#break>
    <#case "ManyToMany"><#assign javaType = "List<Long>"><#break>
    <#case "OneToMany"><#assign javaType = "List<${field.relatedModel}>"><#break>
    <#default>
</#switch>
    @Schema(description = "${field.labelName}")
    private ${javaType} ${field.fieldName};

</#list>
}