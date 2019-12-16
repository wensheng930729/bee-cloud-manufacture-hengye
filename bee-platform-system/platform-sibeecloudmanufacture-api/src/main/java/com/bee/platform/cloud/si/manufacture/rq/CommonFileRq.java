package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;


/**
 * @Description 公共附件信息
 * @author chenxm66777123
 * @Date 2019/9/23 17:24
 * @version 1.0.0
 */
@Data
@NoArgsConstructor
@Accessors(chain = true)
@ApiModel(value = "公共附件信息")
public class CommonFileRq implements Serializable{

    private static final long serialVersionUID = 1L;


    @ApiModelProperty(value = "文件名字")
    private String fileName;

    @ApiModelProperty(value = "文件地址")
    private String fileUrl;

}
