package com.bee.platform.cloud.si.manufacture.controller;

import com.alibaba.fastjson.JSONObject;
import com.bee.platform.cloud.si.manufacture.constants.enums.EnumUploadType;
import com.bee.platform.common.annotation.NotIntercept;
import com.bee.platform.common.entity.AuthPlatformUserInfo;
import com.bee.platform.common.entity.ResCodeEnum;
import com.bee.platform.common.entity.ResponseResult;
import com.bee.platform.common.service.ConfigService;
import com.bee.platform.common.service.OBSFileService;
import com.bee.platform.common.utils.ConstantsUtil;
import com.bee.platform.common.utils.UserInfoUtils;
import io.swagger.annotations.Api;
import io.swagger.annotations.ApiOperation;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import java.util.Arrays;
import java.util.List;

@Api(value = "华为云对象存储相关-API", tags = "华为云对象存储相关-API")
@RestController
@RequestMapping("/file")
@CrossOrigin(origins = "*")
public class OBSFileController {

    @Autowired
    private ConfigService configService;
    @Autowired
    private OBSFileService obsFileService;

    /**
     * @Description 通用文件上传
     * @author chenxm66777123
     * @Date 2019年4月17日
     * @version 1.0.0
     */
    @NotIntercept
    @ApiOperation(value = "文件上传", notes = "文件上传")
    @RequestMapping(value = "/upload", method = RequestMethod.POST)
    public ResponseResult<JSONObject> upload(@RequestPart MultipartFile file, String type) {

        // 验证数据处理

        String[] fileNames = {};
        String originalFilename = "";
        if (null != file.getOriginalFilename()) {
            originalFilename = file.getOriginalFilename();
            if (originalFilename != null && !"".equals(originalFilename)) {
                fileNames = originalFilename.split("\\.");
            }
        }
        // 获取文件后缀名
        String fileExtention = fileNames.length > 1 ? "." + fileNames[fileNames.length - 1].toLowerCase() : "";
        JSONObject json = new JSONObject();

        // 文件上传初始大小限制值
        String uploadInitSize = configService.getConfigByconfigKey("upload_initSize").getConfigValue();

        if (file.getSize() > Integer.valueOf(uploadInitSize) * 1024 * 1024) {
            json.put("message", "文件大小不能超过" + uploadInitSize + "M");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER, json);
        }

        List<String> uploadType = Arrays.asList(ConstantsUtil.UPLOAD_TYPE);

        if (type == null || !uploadType.contains(type)) {
            json.put("message", "上传分类错误或选择的类型为空");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER, json);
        }

        // 限制上传图片类型
        if (EnumUploadType.UPLOAD_TYPE.IMAGE.getKey().equals(type)) {
            List<String> imageFormat = Arrays.asList(ConstantsUtil.IMAGE_FORMAT);
            // 校验文件格式
            if (!StringUtils.isEmpty(fileExtention) && !imageFormat.contains(fileExtention)) {
                json.put("message", "文件扩展名只能为jpg或png! 文件类型：" + fileExtention);
                return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER, json);
            }
        } else if (EnumUploadType.UPLOAD_TYPE.OFFICE.getKey().equals(type)) {
            List<String> officeFormat = Arrays.asList(ConstantsUtil.OFFICE_FORMAT);
            // 校验文件格式
            if (!StringUtils.isEmpty(fileExtention) && !officeFormat.contains(fileExtention)) {
                json.put("message", "文件扩展名只能为xls,xlsx,doc,docx! 文件类型：" + fileExtention);
                return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER, json);
            }
        }

        return obsFileService.upload(file, fileExtention);

    }

    /**
     * @Description 通用文件上传（将word转为pdf保存）
     * @author chenxm66777123
     * @Date 2019年4月17日
     * @version 1.0.0
     */
    @NotIntercept
    @ApiOperation(value = "通用文件上传（将word转为pdf保存）", notes = "通用文件上传（将word转为pdf保存）")
    @RequestMapping(value = "/uploadWordToPdf", method = RequestMethod.POST)
    public ResponseResult<JSONObject> uploadWordToPdf(@RequestPart MultipartFile file) {

        String[] fileNames = {};
        String originalFilename = "";
        if (null != file.getOriginalFilename()) {
            originalFilename = file.getOriginalFilename();
            if (originalFilename != null && !"".equals(originalFilename)) {
                fileNames = originalFilename.split("\\.");
            }
        }
        // 获取文件后缀名
        String fileExtention = fileNames.length > 1 ? "." + fileNames[fileNames.length - 1].toLowerCase() : "";
        JSONObject json = new JSONObject();

        List<String> officePdfFormat = Arrays.asList(ConstantsUtil.OFFICE_PDF_FORMAT);
        // 校验文件格式
        if (!StringUtils.isEmpty(fileExtention) && !officePdfFormat.contains(fileExtention)) {
            json.put("message", "文件扩展名只能为pdf,doc,docx! 文件类型：" + fileExtention);
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER, json);
        }

        // 文件上传初始大小限制值
        String uploadInitSize = configService.getConfigByconfigKey("upload_initSize").getConfigValue();

        if (file.getSize() > Integer.valueOf(uploadInitSize) * 1024 * 1024) {
            json.put("message", "文件大小不能超过" + uploadInitSize + "M");
            return ResponseResult.buildResponseResult(ResCodeEnum.ERROR_PARAMETER, json);
        }

        return obsFileService.uploadWordToPdf(file, fileExtention);
    }

}
