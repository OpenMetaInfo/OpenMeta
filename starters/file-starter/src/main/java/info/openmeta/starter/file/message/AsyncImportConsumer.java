package info.openmeta.starter.file.message;

import info.openmeta.starter.file.dto.ImportTemplateDTO;
import info.openmeta.starter.file.entity.ImportHistory;
import info.openmeta.starter.file.service.FileRecordService;
import info.openmeta.starter.file.service.ImportHistoryService;
import info.openmeta.starter.file.service.ImportService;
import lombok.extern.slf4j.Slf4j;
import org.apache.rocketmq.spring.annotation.RocketMQMessageListener;
import org.apache.rocketmq.spring.core.RocketMQListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import java.io.InputStream;

/**
 * AsyncImportConsumer
 */
@Slf4j
@Component
@RocketMQMessageListener(topic = "${rocketmq.topics.async-import.topic}", consumerGroup = "${rocketmq.topics.async-import.group}")
public class AsyncImportConsumer implements RocketMQListener<ImportTemplateDTO> {

    @Autowired
    private ImportService importService;

    @Autowired
    private FileRecordService fileRecordService;

    @Autowired
    private ImportHistoryService importHistoryService;

    @Override
    public void onMessage(ImportTemplateDTO importTemplateDTO) {
        ImportHistory importHistory = importHistoryService.getById(importTemplateDTO.getHistoryId());
        InputStream inputStream = fileRecordService.downloadStream(importTemplateDTO.getFileId());
        importService.syncImport(importTemplateDTO, inputStream, importHistory);
    }

}
