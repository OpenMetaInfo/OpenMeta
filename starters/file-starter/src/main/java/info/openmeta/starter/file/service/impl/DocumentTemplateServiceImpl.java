package info.openmeta.starter.file.service.impl;

import com.deepoove.poi.XWPFTemplate;
import com.deepoove.poi.config.Configure;
import com.deepoove.poi.config.ConfigureBuilder;
import com.deepoove.poi.plugin.table.LoopRowTableRenderPolicy;
import info.openmeta.framework.base.exception.SystemException;
import info.openmeta.framework.base.utils.Assert;
import info.openmeta.framework.orm.enums.FileType;
import info.openmeta.framework.orm.service.ModelService;
import info.openmeta.framework.orm.service.impl.EntityServiceImpl;
import info.openmeta.framework.web.dto.FileInfo;
import info.openmeta.starter.file.dto.UploadFileDTO;
import info.openmeta.starter.file.entity.DocumentTemplate;
import info.openmeta.starter.file.service.DocumentTemplateService;
import info.openmeta.starter.file.service.FileRecordService;
import org.docx4j.Docx4J;
import org.docx4j.openpackaging.exceptions.Docx4JException;
import org.docx4j.openpackaging.packages.WordprocessingMLPackage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.io.*;
import java.util.Map;

/**
 * DocumentTemplate Model Service Implementation
 */
@Service
public class DocumentTemplateServiceImpl extends EntityServiceImpl<DocumentTemplate, Long> implements DocumentTemplateService {

    private static final Configure configure;

    @Autowired
    private ModelService<Serializable> modelService;

    @Autowired
    private FileRecordService fileRecordService;

    static  {
        ConfigureBuilder builder = Configure.builder();
        // Custom the mark syntax to ${}
        builder.buildGramer("${", "}");
        // Enable the spring EL
        builder.useSpringEL();
        // Custom the table render policy
        LoopRowTableRenderPolicy policy = new LoopRowTableRenderPolicy();
        builder.addPlugin('>', policy);
        configure = builder.build();
    }

    /**
     * Generate a document according to the specified template ID and row ID.
     *
     * @param templateId template ID
     * @param rowId row ID
     * @return generated document fileInfo with download URL
     */
    @Override
    public FileInfo generateDocument(Long templateId, Serializable rowId) {
        DocumentTemplate template = this.readOne(templateId);
        this.validateTemplate(template);
        Map<String, Object> data = modelService.readOne(template.getModelName(), rowId);
        return generateDocument(templateId, data);
    }

    /**
     * Generate a document according to the specified template ID and data object.
     * The data object could be a map or a POJO.
     *
     * @param templateId template ID
     * @param data the data object to render the document
     * @return generated document fileInfo with download URL
     */
    @Override
    public FileInfo generateDocument(Long templateId, Object data) {
        DocumentTemplate template = this.readOne(templateId);
        this.validateTemplate(template);
        return generateDocumentByTemplate(template, data);
    }

    /**
     * Validate the document template
     * @param template the document template
     */
    private void validateTemplate(DocumentTemplate template) {
        Assert.notNull(template, "The document template does not exist");
        Assert.notBlank(template.getModelName(), "The modelName of `{0}` template is empty", template.getFileName());
        Assert.notNull(template.getFileId(), "The document template file is empty");
    }

    /**
     * Generate a document according to the specified template ID and data object.
     * The data object could be a map or a POJO.
     *
     * @param template the document template
     * @param data the data object to render the document
     * @return generated document fileInfo with download URL
     */
    private FileInfo generateDocumentByTemplate(DocumentTemplate template, Object data) {
        try (InputStream templateInputStream = fileRecordService.downloadStream(template.getFileId());
             ByteArrayOutputStream docOutputStream = new ByteArrayOutputStream()
        ) {
            // render the document from the template
            renderDocument(templateInputStream, data, docOutputStream);
            byte[] docxContent = docOutputStream.toByteArray();
            if (Boolean.TRUE.equals(template.getConvertToPdf())) {
                docxContent = convertDocxToPdf(docxContent);
            }
            // save the generated document to the file record
            try (InputStream docxInputStream = new ByteArrayInputStream(docxContent)) {
                // Construct the uploadFileDTO
                UploadFileDTO uploadFileDTO = new UploadFileDTO();
                uploadFileDTO.setModelName(template.getModelName());
                uploadFileDTO.setFileName(template.getFileName());
                uploadFileDTO.setFileType(FileType.XLSX);
                uploadFileDTO.setFileSize(docxContent.length);
                uploadFileDTO.setInputStream(docxInputStream);
                return fileRecordService.uploadFileToDownload(uploadFileDTO);
            }
        } catch (Exception e) {
            throw new SystemException("Failed to generate the document", e);
        }
    }

    /**
     * Render the document from the template
     *
     * @param templateInputStream the input stream of the template
     * @param data the data object to render the document
     * @param outputStream the output stream of the document
     */
    public static void renderDocument(InputStream templateInputStream, Object data, OutputStream outputStream) {
        try (XWPFTemplate xwpfTemplate = XWPFTemplate.compile(templateInputStream, configure)) {
            xwpfTemplate.render(data);
            xwpfTemplate.write(outputStream);
        } catch (IOException e) {
            throw new SystemException("Failed to render the document from template.", e);
        }
    }

    /**
     * Convert the docx file to PDF
     *
     * @param docxContent the content of the docx file
     * @return the content of the PDF file
     */
    public static byte[] convertDocxToPdf(byte[] docxContent) {
        try (ByteArrayInputStream docInputForPdf = new ByteArrayInputStream(docxContent);
             ByteArrayOutputStream pdfOutputStream = new ByteArrayOutputStream()) {
            convertDocxToPdf(docInputForPdf, pdfOutputStream);
            return pdfOutputStream.toByteArray();
        } catch (Docx4JException | IOException e) {
            throw new SystemException("Failed to convert the document to PDF", e);
        }
    }

    /**
     * Convert the docx file to PDF
     *
     * @param docxInputStream the input stream of the docx file
     * @param pdfOutputStream the output stream of the PDF file
     * @throws Docx4JException if the conversion fails
     */
    public static void convertDocxToPdf(InputStream docxInputStream, OutputStream pdfOutputStream) throws Docx4JException {
        // Load the docx file
        WordprocessingMLPackage wordMLPackage = WordprocessingMLPackage.load(docxInputStream);
        // Convert the docx file to PDF
        Docx4J.toPDF(wordMLPackage, pdfOutputStream);
    }
}