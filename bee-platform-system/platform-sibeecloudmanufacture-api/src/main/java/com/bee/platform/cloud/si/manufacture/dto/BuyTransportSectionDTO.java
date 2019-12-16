package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.util.List;

/**
 * @ClassName BuyTransportSectionDTO
 * @Description 物流批次运输段信息
 * @author qh.wang
 * @version 1.0.0
 * @Date 2019/9/24 15:15
 */
@Data
@Accessors(chain = true)
@ApiModel("物流批次运输段信息")
public class BuyTransportSectionDTO {

    @ApiModelProperty("id")
    private Long id;

    @ApiModelProperty("批次ID")
    private String batchId;

    @ApiModelProperty("采购合同业务id")
    private String contractBusinessId;

    @ApiModelProperty("运输段id")
    private String transportSectionId;

    @ApiModelProperty("运输段")
    private Integer transportSection;

    @ApiModelProperty("第几段运输")
    private String transportSectionName;

    @ApiModelProperty("运输方式(1-汽车 2-轮船 3-火车)")
    private Integer transportMode;

    @ApiModelProperty("运输方式描述(1-汽车 2-轮船 3-火车)")
    private String transportModeName;

    @ApiModelProperty("是否到厂(0-不到厂 1-到厂)")
    private Integer toFactory;

    @ApiModelProperty("是否到厂描述(0-不到厂 1-到厂)")
    private String toFactoryName;

    @ApiModelProperty("起始地地点id")
    private Integer startingPlaceId;

    @ApiModelProperty("起始地")
    private String startingPlace;

    @ApiModelProperty("到达地地点id")
    private Integer arrivalPlaceId;

    @ApiModelProperty("到达地")
    private String arrivalPlace;

    @ApiModelProperty("运输段承运方信息")
    private List<BuyCarrierTransportDTO> carrierTransportDTOS;

}
