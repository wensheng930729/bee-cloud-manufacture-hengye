package com.bee.platform.common.utils;

import com.aspose.words.Document;
import com.aspose.words.FontSettings;
import com.aspose.words.License;
import com.aspose.words.SaveFormat;
import org.apache.commons.io.IOUtils;
import org.aspectj.weaver.ast.Test;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;

/**
 * 
 * @Description word转pdf
 * @author chenxm66777123
 * @Date 2019年4月17日
 * @version 1.0.0
 */
public class Word2PdfUtil {

	public static boolean getLicense() {
		boolean result = false;
		try {
			// license.xml应放在..\WebRoot\WEB-INF\classes路径下
			InputStream is = Test.class.getClassLoader().getResourceAsStream("license.xml");
			License aposeLic = new License();
			aposeLic.setLicense(is);
			result = true;
		} catch (Exception e) {
			e.printStackTrace();
		}
		return result;
	}

	public static void doc2pdf(String tempWordFilePath, String tempPdfFilePath) {
		// 验证License 若不验证则转化出的pdf文档会有水印产生
		if (!getLicense()) {
			return;
		}
		FileOutputStream os = null;
		try {
			File file = new File(tempPdfFilePath);
			if (!file.exists()) {
				try {
					file.createNewFile();
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
			os = new FileOutputStream(file);
			 
			//设置多个字体目录

			FontSettings.setFontsFolder("/usr/share/fonts/winFonts", false);
			// Address是将要被转化的word文档
			Document doc = new Document(tempWordFilePath);
			// 全面支持DOC, DOCX, OOXML, RTF HTML, OpenDocument, PDF,EPUB, XPS, SWF 相互转换
			doc.save(os, SaveFormat.PDF);
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			IOUtils.closeQuietly(os);
		}
	}
}
