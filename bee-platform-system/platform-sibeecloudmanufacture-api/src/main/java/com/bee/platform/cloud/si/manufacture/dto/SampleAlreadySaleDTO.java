package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * @author liang.li
 * @ClassName SampleAlreadySaleDTO
 * @Description 销售已取样列表DTO
 * @Date 2019/9/23
 */
@Data
@Accessors(chain = true)
@ApiModel(value = "销售已取样列表DTO")
public class SampleAlreadySaleDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty(value = "业务类型1采购2销售3生产")
    private Integer businessType;

    @ApiModelProperty(value = "样品编号")
    private String sampleCode;

    @ApiModelProperty(value = "样品id")
    private Integer productId;

    @ApiModelProperty(value = "样品名称")
    private String productName;

    @ApiModelProperty(value = "取样时间")
    private Date sampleTime;

    @ApiModelProperty(value = "取样人")
    private String samplePerson;

    @ApiModelProperty(value = "吨袋编码列表")
    private List<String> tonCodeList;

}
