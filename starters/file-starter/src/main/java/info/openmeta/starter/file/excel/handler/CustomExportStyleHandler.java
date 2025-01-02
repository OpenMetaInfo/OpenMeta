package info.openmeta.starter.file.excel.handler;

import com.alibaba.excel.write.handler.RowWriteHandler;
import com.alibaba.excel.write.handler.context.RowWriteHandlerContext;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;

/**
 * custom export style handler
 * head color
 */
public class CustomExportStyleHandler implements RowWriteHandler {

    @Override
    public void afterRowDispose(RowWriteHandlerContext context) {
        if (!context.getHead()) {
            return;
        }

        Sheet sheet = context.getWriteSheetHolder().getSheet();
        Workbook workbook = context.getWriteWorkbookHolder().getWorkbook();
        // Set column width
        sheet.setDefaultColumnWidth(25);

        // Set the header row style
        Row titleRow = sheet.getRow(0);
        // Set row height
        titleRow.setHeightInPoints((short) 30);
        // Set header row font style
        Font titleFont = workbook.createFont();
        titleFont.setBold(true);
        titleFont.setFontName("宋体");
        titleFont.setColor(IndexedColors.WHITE.getIndex());
        XSSFCellStyle titleStyle = (XSSFCellStyle) workbook.createCellStyle();
        titleStyle.setFont(titleFont);
        titleStyle.setFillForegroundColor(IndexedColors.DARK_TEAL.getIndex());
        titleStyle.setFillPattern(FillPatternType.SOLID_FOREGROUND);
        titleStyle.setAlignment(HorizontalAlignment.CENTER);
        titleStyle.setVerticalAlignment(VerticalAlignment.CENTER);

        // Apply the default style to header row
        for (int i = 0; i < titleRow.getLastCellNum(); i++) {
            Cell cell = titleRow.getCell(i);
            cell.setCellStyle(titleStyle);
        }
    }

}
