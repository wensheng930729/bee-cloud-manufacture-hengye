package com.bee.platform.common.service;

import com.alibaba.fastjson.JSONObject;
import com.bee.platform.common.entity.ResponseResult;
import org.springframework.web.multipart.MultipartFile;

/**
 * @author chenxm66777123
 * @version 1.0.0
 * @Description 华为云文件上传处理接口
 * @Date 2019-09-23
 */
public interface OBSFileService {


    /**
     * @Description
     * @author chenxm66777123
     * @Date 2019年5月9日
     * @version 1.0.0
     */
    ResponseResult<JSONObject> upload(MultipartFile file, String fileExtention);

    /**
     * @Description 通用文件上传（将word转为pdf保存）
     * @author chenxm66777123
     * @Date 2019年5月9日
     * @version 1.0.0
     */
    ResponseResult<JSONObject> uploadWordToPdf(MultipartFile file, String fileExtention);
}
