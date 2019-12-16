package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @author liang.li
 * @ClassName SampleMachineBuyDTO
 * @Description 根据车牌号查询磅单信息DTO
 * @Date 2019/9/23
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "根据车牌号查询磅单信息DTO")
public class SampleMachineBuyDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "磅单业务id")
    private String machineId;

    @ApiModelProperty(value = "合同业务id")
    private String contractBusinessId;

    @ApiModelProperty(value = "车牌号")
    private String trainNumber;

    @ApiModelProperty(value = "产品ID")
    private Integer productId;

    @ApiModelProperty(value = "产品名称")
    private String productName;

    @ApiModelProperty(value = "称重日期日期")
    private Date weighingTime;

    @ApiModelProperty(value = "称重日期")
    private String weightDate;

    @ApiModelProperty(value = "称重时间")
    private String weightTime;


}
