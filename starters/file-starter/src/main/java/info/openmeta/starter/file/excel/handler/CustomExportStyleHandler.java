package info.openmeta.starter.file.excel.handler;

import com.alibaba.excel.write.handler.CellWriteHandler;
import com.alibaba.excel.write.handler.context.CellWriteHandlerContext;
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;

import java.util.HashMap;
import java.util.Map;

/**
 * custom export style handler
 * head color
 */
public class CustomExportStyleHandler implements CellWriteHandler {

    /**
     * Mix styles to reduce repetitive generation styles
     */
    private Map<String, CellStyle> styleCache = new HashMap<>();

    @Override
    public void afterCellDispose(CellWriteHandlerContext context) {
        Sheet sheet = context.getWriteSheetHolder().getSheet();
        Workbook workbook = context.getWriteWorkbookHolder().getWorkbook();
        // Set column width
        sheet.setDefaultColumnWidth(25);

        // Set the global font to Microsoft Yahei
        Font defaultFont = workbook.createFont();
        defaultFont.setFontName("宋体");
        CellStyle defaultStyle = workbook.createCellStyle();
        defaultStyle.setFont(defaultFont);
        styleCache.put("default", defaultStyle);

        // Sets the header row style
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
        styleCache.put("title", titleStyle);

        // Apply the default style to header row
        for (int i = 0; i < titleRow.getLastCellNum(); i++) {
            Cell cell = titleRow.getCell(i);
            cell.setCellStyle(titleStyle);
        }

        // Apply the default style to all cells
        for (int rowIndex = 1; rowIndex <= sheet.getLastRowNum(); rowIndex++) {
            Row row = sheet.getRow(rowIndex);
            if (row != null) {
                for (int cellIndex = 0; cellIndex < row.getLastCellNum(); cellIndex++) {
                    Cell cell = row.getCell(cellIndex);
                    if (cell != null) {
                        cell.setCellStyle(defaultStyle);
                    }
                }
            }
        }
    }

}
