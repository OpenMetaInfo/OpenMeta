package info.openmeta.starter.designer.generator;

import info.openmeta.framework.orm.utils.BeanTool;
import info.openmeta.starter.designer.dto.ModelCodeDTO;
import info.openmeta.framework.base.context.Context;
import info.openmeta.framework.base.context.ContextHolder;
import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.orm.utils.MapUtils;
import freemarker.template.Configuration;
import freemarker.template.Template;
import freemarker.template.TemplateException;
import freemarker.template.TemplateExceptionHandler;
import info.openmeta.starter.designer.entity.DesignModel;

import java.io.IOException;
import java.io.StringWriter;
import java.time.LocalDate;
import java.util.Map;

/**
 * Code generator, generates Java class code for the model's Entity, Service, ServiceImpl, and Controller
 */

public class CodeGenerator {

    private static final Configuration freemarkerConfig = new Configuration(Configuration.VERSION_2_3_28);

    static {
        freemarkerConfig.setClassForTemplateLoading(CodeGenerator.class, "/templates/code");
        freemarkerConfig.setDefaultEncoding("UTF-8");

        // Throw an exception when an error occurs
        freemarkerConfig.setTemplateExceptionHandler(TemplateExceptionHandler.RETHROW_HANDLER);

        // Close template cache when templates increase continuously to reduce memory usage
        // configuration.setCacheStorage(null);

        // Disable template update check, changes to templates require a restart to take effect
        freemarkerConfig.setTemplateUpdateDelayMilliseconds(Long.MAX_VALUE);

        // Disable automatic recognition of shared variables
        freemarkerConfig.setRecognizeStandardFileExtensions(false);

        // Minimize output formatting
        // freemarkerConfig.setOutputFormat(HTMLOutputFormat.INSTANCE);
    }

    /**
     * Set additional parameters: username, current date
     *
     * @return Additional parameters
     */
    private static Map<String, Object> getAdditionalParams() {
        Context context = ContextHolder.getContext();
        return MapUtils.strObj()
                .put("userName", context.getName())
                .put("currentDate", LocalDate.now())
                .build();
    }

    /**
     * Generate corresponding code based on the template filename
     *
     * @param templateFilename Template filename
     * @param params Parameter data
     * @return Generated code
     */
    public static String generate(String templateFilename, Map<String, Object> params) {
        try {
            Template template = freemarkerConfig.getTemplate(templateFilename);
            StringWriter result = new StringWriter();
            template.process(params, result);
            return result.toString();
        } catch (IOException | TemplateException exception) {
            throw new IllegalArgumentException(exception.getMessage());
        }
    }

    /**
     * Use the specified template file to generate model's code:
     *      Entity, Service, ServiceImpl, Controller class code.
     *
     * @param packageName Package name
     * @param designModel Model with fields
     * @return Model code object
     */
    public static ModelCodeDTO generateModelCode(String packageName, DesignModel designModel) {
        Map<String, Object> modelData = BeanTool.objectToMap(designModel);
        modelData.put("packageName", packageName);
        modelData.putAll(getAdditionalParams());
        // Generate model code
        ModelCodeDTO modelCode = new ModelCodeDTO(designModel.getModelName(), packageName);
        modelCode.setEntityCode(generate("Entity.ftl", modelData));
        modelCode.setServiceCode(generate("Service.ftl", modelData));
        modelCode.setServiceImplCode(generate("ServiceImpl.ftl", modelData));
        modelCode.setControllerCode(generate("Controller.ftl", modelData));
        return modelCode;
    }
}