package info.openmeta.framework.web.controller;

import info.openmeta.framework.web.controller.vo.ModelField;
import info.openmeta.framework.web.controller.vo.ModelFields;
import info.openmeta.framework.web.response.ApiResponse;
import info.openmeta.framework.web.service.ToolkitService;
import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.Operation;
import jakarta.validation.Valid;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
* Toolkit controller
*/
@Tag(name = "Toolkit")
@RestController
@RequestMapping("/toolkit")
public class ToolkitController {

    @Autowired
    private ToolkitService toolkitService;

    /**
     * Recompute the stored calculation fields, including computed and cascaded fields.
     *
     * @param modelFields the model name and the list of fields to be recomputed
     */
    @Operation(summary = "recompute", description = """
            Recomputes the stored computed and cascaded fields for the specified model.
            If fields are not specified, all stored computed and cascaded fields are recomputed by default.""")
    @PostMapping("/recompute")
    public ApiResponse<Boolean> recompute(@RequestBody @Valid ModelFields modelFields) {
        toolkitService.recompute(modelFields.getModel(), modelFields.getFields());
        return ApiResponse.success(true);
    }

    /**
     * Encrypts historical plaintext data after the field is set to `encrypted=true`.
     *
     * @param modelField the model name and the field name for which historical data needs to be encrypted
     * @return the number of rows fixed
     */
    @Operation(summary = "fixUnencryptedData: Fix Unencrypted Data", description = """
            After changing field to encrypted field, call this API to encrypt historical plaintext data.
            The field name cannot be empty. Returns the number of rows fixed.""")
    @PostMapping("/fixUnencryptedData")
    public ApiResponse<Long> fixUnencryptedData(@RequestBody @Valid ModelField modelField) {
        Long result = toolkitService.fixUnencryptedData(modelField.getModel(), modelField.getField());
        return ApiResponse.success(result);
    }
}