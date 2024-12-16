package info.openmeta.starter.designer.dto;

import info.openmeta.framework.base.exception.BusinessException;
import info.openmeta.framework.orm.utils.MapUtils;
import io.swagger.v3.oas.annotations.media.Schema;
import lombok.Data;

import java.util.Map;

/**
 * DTO for model code file
 */
@Data
@Schema(name = "Model Code")
public class ModelCodeDTO {

    @Schema(description = "Model name")
    private String modelName;

    @Schema(description = "Package name")
    private String packageName;

    @Schema(description = "Model entity code")
    private String entityCode;

    @Schema(description = "Model Service code")
    private String serviceCode;

    @Schema(description = "Service implementation code")
    private String serviceImplCode;

    @Schema(description = "Controller code")
    private String controllerCode;

    public ModelCodeDTO(String modelName, String packageName) {
        this.modelName = modelName;
        this.packageName = packageName;
    }

    /**
     * Get the mapping of relative path file names to corresponding code text: relativeFileName -> fileContent
     */
    public Map<String, String> fileCodeMap() {
        String relativePath = modelName + "/" + packageName.replace(".", "/");
        return MapUtils.strStr()
                .put(relativePath + "/entity/" + modelName + ".java", entityCode)
                .put(relativePath + "/service/" + modelName + "Service.java", serviceCode)
                .put(relativePath + "/service/impl/" + modelName + "ServiceImpl.java", serviceImplCode)
                .put(relativePath + "/controller/" + modelName + "Controller.java", controllerCode)
                .build();
    }

    /**
     * Get a string array composed of the file name and code text for the specified file type [fileName, fileContent]
     * The file type only supports: entity, service, serviceImpl, controller, which are the four attribute types of the current object
     */
    public String[] getFileCode(String fileType) {
        return switch (fileType) {
            case "entity" -> new String[]{modelName + ".java", entityCode};
            case "service" -> new String[]{modelName + "Service.java", serviceCode};
            case "serviceImpl" -> new String[]{modelName + "ServiceImpl.java", serviceImplCode};
            case "controller" -> new String[]{modelName + "Controller.java", controllerCode};
            default -> throw new BusinessException("""
                    The file type for code generation only supports: entity, service, serviceImpl, controller.
                    The file type passed as a parameter is not supported: {0}""", fileType);
        };
    }
}