package com.bee.platform.common.service.impl;

import com.alibaba.fastjson.JSON;
import com.alibaba.fastjson.JSONObject;
import com.bee.platform.common.dto.QCloudDTO;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.service.OBSFileService;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.Word2PdfUtil;
import com.obs.services.ObsClient;
import com.obs.services.exception.ObsException;
import com.obs.services.model.HeaderResponse;
import lombok.extern.slf4j.Slf4j;
import org.springframework.core.io.ClassPathResource;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import java.io.File;
import java.io.IOException;
import java.util.Properties;
import java.util.UUID;

/**
 * @author chenxm66777123
 * @version 1.0.0
 * @Description 文件上传处理接口实现类
 * @Date 2019-09-23
 */
@Slf4j
@Service
public class OBSFileServiceImpl implements OBSFileService {

    /**
     * @Description 加速节点
     */
    private String endPoint;

    /**
     * @Description AccessKeyId（AK）： 访问密钥ID。与私有访问密钥关联的唯一标识符；访问密钥ID和私有访问密钥一起使用，对请求进行加密签名。
     */
    private String ak;

    /**
     * @Description Secret Access Key（SK）：与访问密钥ID结合使用的密钥，对请求进行加密签名，可标识发送方，并防止请求被修改。
     */
    private String sk;

    /**
     * @Description 对象桶
     */
    private String bucketName;

    /**
     * @Description 临时文件存储地址
     */
    private String imageLocalpath;

    /**
     * @Description pdf后缀
     */
    private final String PDF_FILE_EXTENTION = ".pdf";

    /**
     * @Description 通用文件上传
     * @author chenxm66777123
     * @Date 2019年5月9日
     * @version 1.0.0
     */
    @Override
    public ResponseResult<JSONObject> upload(MultipartFile file, String fileExtention) {
        //构造环境
        try {
            buildOBSEnvironment();
        } catch (IOException e) {
            e.printStackTrace();
        }
        // 本机运行开启
        //imageLocalpath = "F:/ImgTest/";
        String fileUUid = UUID.randomUUID().toString().replace("-", "");
        File tempFile = new File(imageLocalpath + fileUUid + fileExtention);
        tempFile = judgeFileExists(file, tempFile);
        return this.fileToOBS(tempFile, fileUUid, fileExtention, true);
    }

    /**
     * @Description 通用文件上传（将word转为pdf保存）
     * @author chenxm66777123
     * @Date 2019年5月9日
     * @version 1.0.0
     */
    @Override
    public ResponseResult<JSONObject> uploadWordToPdf(MultipartFile file, String fileExtention) {
        //构造环境
        try {
            buildOBSEnvironment();
        } catch (IOException e) {
            e.printStackTrace();
        }
        JSONObject json = new JSONObject();
        // 本机运行开启
        // imageLocalpath = "F:/ImgTest/";
        String fileUUid = UUID.randomUUID().toString().replace("-", "");
        String tempFilePath = imageLocalpath + fileUUid + fileExtention;
        File tempFile = new File(tempFilePath);
        tempFile = judgeFileExists(file, tempFile);
        // 首先保存word文档
        ResponseResult<JSONObject> resultWord = this.fileToOBS(tempFile, fileUUid, fileExtention, false);
        json.put("wordDate", resultWord.getObject());
        if (!".doc".equals(fileExtention) && !".docx".equals(fileExtention)) {
            json.put("pdfDate", resultWord.getObject());
            if (tempFile.exists()) {
                tempFile.delete();
            }
            return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, json);
        }
        String tempPdfFilePath = imageLocalpath + fileUUid + PDF_FILE_EXTENTION;
        Word2PdfUtil.doc2pdf(tempFilePath, tempPdfFilePath);
        File tempPdfFile = new File(tempPdfFilePath);
        ResponseResult<JSONObject> resultPdf = this.fileToOBS(tempPdfFile, fileUUid, PDF_FILE_EXTENTION, true);
        json.put("pdfDate", resultPdf.getObject());
        if (tempFile.exists()) {
            tempFile.delete();
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, json);
    }

    /**
     * @Description 判断文件是否存在
     * @author chenxm66777123
     * @Date 2019/9/23 16:23
     * @version 1.0.0
     */
    private File judgeFileExists(MultipartFile file, File tempFile) {
        if (!tempFile.exists()) {
            try {
                tempFile.createNewFile();
                file.transferTo(tempFile);
                return tempFile;
            } catch (IOException e) {
                e.printStackTrace();
            }
        }
        return null;
    }

    /**
     * @Description 文件上传到华为云对象存储服务器上
     * @author chenxm66777123
     * @Date 2019年4月17日
     * @version 1.0.0
     */
    public ResponseResult<JSONObject> fileToOBS(File tempFile, String fileUUid, String fileExtention,
                                                boolean openDelete) {
        JSONObject json = new JSONObject();
        // 您的工程中可以只保留一个全局的ObsClient实例
        // ObsClient是线程安全的，可在并发场景下使用
        ObsClient obsClient = null;
        // ak ----- Y5C8SVCPHA3SUGK0FTQK
        // sk --- xIVglSyrKMkiq8mmRxHQZT7rfqCIcBguM8LcsajA
        try {
            // 创建ObsClient实例
            obsClient = new ObsClient(ak, sk, endPoint);
            // 调用接口进行操作，例如上传对象
            HeaderResponse response = obsClient.putObject(bucketName, imageLocalpath + fileUUid + fileExtention,
                    tempFile);
            JSONObject responseJson = JSONObject.parseObject(JSON.toJSONString(response));
            // 获取文件上传的地址
            String objectUrl = responseJson.getString(ConstantsUtil.OBJECTURL);
            String etag = responseJson.getString(ConstantsUtil.ETAG);

            // 组装成为腾讯云一样的参数类型,并返回
            QCloudDTO qCloudDTO = new QCloudDTO();
            qCloudDTO.setVid(etag);
            qCloudDTO.setAccess_url(objectUrl);
            qCloudDTO.setResource_path(objectUrl);
            qCloudDTO.setSource_url(objectUrl);
            qCloudDTO.setUrl(objectUrl);

            json = JSONObject.parseObject(JSON.toJSONString(qCloudDTO));
            json.put("fileToOBS", "新的文件上传接口fileToOBS");

        } catch (ObsException e) {
            log.info("Response Code {}" + e.getResponseCode());
            log.info("Error Message {}" + e.getErrorMessage());
            log.info("Error Code {}" + e.getErrorCode());
            log.info("Request ID {}" + e.getErrorRequestId());
            log.info("Host ID {}" + e.getErrorHostId());
        } finally {
            // 关闭ObsClient实例，如果是全局ObsClient实例，可以不在每个方法调用完成后关闭
            // ObsClient在调用ObsClient.close方法关闭后不能再次使用
            if (obsClient != null) {
                try {
                    obsClient.close();
                } catch (IOException e) {
                }
            }
            if (openDelete && tempFile.exists()) {
                tempFile.delete();
            }
        }
        return ResponseResult.buildResponseResult(ResCodeEnum.SUCCESS, json);

    }

    /**
     * @Description 构建文件上传环境
     * @author chenxm66777123
     * @Date 2019/9/23 16:46
     * @version 1.0.0
     */
    private void buildOBSEnvironment() throws IOException {
        Properties properties = new Properties();
        ClassPathResource resource = new ClassPathResource("obsfile.properties");
        properties.load(resource.getInputStream());
        this.endPoint =  properties.getProperty("endPoint");
        this.ak =  properties.getProperty("ak");
        this.sk =  properties.getProperty("sk");
        this.bucketName =  properties.getProperty("bucketName");
        this.imageLocalpath =  properties.getProperty("imageLocalpath");
    }
}
