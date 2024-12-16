package info.openmeta.starter.designer.controller;

import info.openmeta.framework.base.exception.IllegalArgumentException;
import info.openmeta.framework.base.utils.StringTools;
import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.framework.web.response.ApiResponse;
import info.openmeta.starter.designer.dto.ModelCodeDTO;
import info.openmeta.starter.designer.entity.DesignModel;
import info.openmeta.starter.designer.service.DesignModelService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.Parameters;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.util.Map;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

/**
 * DesignModel Model Controller
 */
@Tag(name = "DesignModel")
@RestController
@RequestMapping("/DesignModel")
public class DesignModelController extends EntityController<DesignModelService, DesignModel, Long> {

    /**
     * Preview the DDL SQL of model, including table creation and index creation
     *
     * @param id Model ID
     * @return Model DDL SQL
     */
    @Operation(description = "Preview the DDL SQL of model, including table creation and index creation")
    @GetMapping(value = "/previewDDL")
    @Parameter(name = "id", description = "Model ID")
    public ApiResponse<String> previewDDL(@RequestParam Long id) {
        return ApiResponse.success(service.previewDDL(id));
    }

    /**
     * Preview the generated model code, including Java class code for Entity, Service, ServiceImpl, and Controller
     *
     * @param id Model ID
     * @return Current model code text
     */
    @Operation(description = "Preview the generated model code, including Java class code for Entity, Service, ServiceImpl, and Controller")
    @GetMapping(value = "/previewCode")
    @Parameter(name = "id", description = "Model ID")
    public ApiResponse<ModelCodeDTO> previewCode(@RequestParam Long id) {
        return ApiResponse.success(service.previewCode(id));
    }

    /**
     * Download the specified Java code file
     *
     * @param id Model ID
     * @param fileType File type, corresponding to ModelCodeDTO properties: entity, service, serviceImpl, controller
     * @return Single Java code file
     */
    @Operation(description = "Download specified code file")
    @GetMapping(value = "/downloadCode")
    @Parameters({
            @Parameter(name = "id", description = "Model ID"),
            @Parameter(name = "fileType", description = "File type, supporting 'entity', 'service', 'serviceImpl', 'controller'.")
    })
    public ResponseEntity<byte[]> downloadCode(@RequestParam Long id, @RequestParam String fileType) {
        String[] fileCode = service.previewCode(id).getFileCode(StringTools.toCamelCase(fileType));
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_OCTET_STREAM);
        headers.setContentDispositionFormData("attachment", fileCode[0]);
        return new ResponseEntity<>(fileCode[1].getBytes(), headers, HttpStatus.OK);
    }

    /**
     * Download the model Java code zip package
     *
     * @param id Model ID
     * @return Model Java code zip package
     */
    @Operation(description = "Download model code package")
    @GetMapping(value = "/downloadZip")
    @Parameter(name = "id", description = "Model ID")
    public ResponseEntity<byte[]> downloadZip(@RequestParam Long id) {
        ModelCodeDTO modelCodeDTO = service.previewCode(id);
        ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
        try (ZipOutputStream zipOutputStream = new ZipOutputStream(outputStream)) {
            for (Map.Entry<String, String> entry : modelCodeDTO.fileCodeMap().entrySet()) {
                String relativeFileName = entry.getKey();
                String fileContent = entry.getValue();
                zipOutputStream.putNextEntry(new ZipEntry(relativeFileName));
                zipOutputStream.write(fileContent.getBytes());
                zipOutputStream.closeEntry();
            }
        } catch (IOException e) {
            throw new IllegalArgumentException("Failed to generate file package of model {0}:", modelCodeDTO.getModelName(), e);
        }
        // Set response headers and Zip package file name: modelName.zip
        String zipFileName = modelCodeDTO.getModelName() + ".zip";
        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_OCTET_STREAM);
        headers.setContentDispositionFormData("attachment", zipFileName);
        return new ResponseEntity<>(outputStream.toByteArray(), headers, HttpStatus.OK);
    }
}