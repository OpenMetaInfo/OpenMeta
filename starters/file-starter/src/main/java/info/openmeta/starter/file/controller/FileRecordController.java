package info.openmeta.starter.file.controller;

import info.openmeta.framework.web.controller.EntityController;
import info.openmeta.starter.file.entity.FileRecord;
import info.openmeta.starter.file.service.FileRecordService;
import io.swagger.v3.oas.annotations.tags.Tag;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

/**
 * FileRecordController
 */
@Tag(name = "File Record")
@RestController
@RequestMapping("/FileRecord")
public class FileRecordController extends EntityController<FileRecordService, FileRecord, Long> {

}