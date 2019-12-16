package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;

/**
 * @author liang.li
 * @ClassName SampleAlreadyBuyDTO
 * @Description 采购已取样列表DTO
 * @Date 2019/9/23
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "采购已取样列表DTO")
public class SampleAlreadyBuyDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "业务类型1采购2销售3生产")
    private Integer businessType;

    @ApiModelProperty(value = "样品编号")
    private String sampleCode;

    @ApiModelProperty(value = "车牌号")
    private String trainNumber;

    @ApiModelProperty(value = "磅单业务id")
    private String machineId;

    @ApiModelProperty(value = "取样时间")
    private Date sampleTime;

    @ApiModelProperty(value = "样品名称")
    private String productName;

    @ApiModelProperty(value = "取样人")
    private String samplePerson;



}
