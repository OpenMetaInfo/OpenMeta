package info.openmeta.starter.file.excel.handler;

import com.alibaba.excel.write.handler.CellWriteHandler;
import com.alibaba.excel.write.handler.context.CellWriteHandlerContext;
import com.alibaba.excel.write.metadata.style.WriteCellStyle;
import com.alibaba.excel.write.metadata.style.WriteFont;
import org.apache.poi.ss.usermodel.*;
import org.springframework.util.CollectionUtils;

import java.util.List;

/**
 * Custom header style
 */
public class CustomHeadStyleHandler implements CellWriteHandler {

    // dynamic required headers
    private final List<String> requiredHeaderList;

    public CustomHeadStyleHandler(List<String> requiredHeaderList) {
        this.requiredHeaderList = requiredHeaderList;
    }

    @Override
    public void afterCellDispose(CellWriteHandlerContext context) {
        if (!CollectionUtils.isEmpty(requiredHeaderList) && context.getHead()) {
            if (requiredHeaderList.contains(context.getHeadData().getHeadNameList().getFirst())) {
                WriteCellStyle writeCellStyle = context.getFirstCellData().getOrCreateStyle();
                // Merges existing cell styles
                WriteFont customFont = new WriteFont();
                WriteFont.merge(writeCellStyle.getWriteFont(), customFont);
                customFont.setColor(IndexedColors.RED.getIndex());
                writeCellStyle.setWriteFont(customFont);
            }
        }
    }

}
