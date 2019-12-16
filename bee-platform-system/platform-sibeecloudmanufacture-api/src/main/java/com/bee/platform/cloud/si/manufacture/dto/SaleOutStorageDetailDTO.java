package com.bee.platform.cloud.si.manufacture.dto;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.experimental.Accessors;

import java.math.BigDecimal;
import java.util.List;

/**
 * @ClassName: SaleOutStorageDetailDTO
 * @Description: 销售出库详情实体类
 * @Author: fei.sun
 * @Date: 2019/10/6 11:10
 * @Version: 1.0
 */

@Data
@Accessors(chain = true)
@ApiModel("销售出库详情实体类")
public class SaleOutStorageDetailDTO {

    @ApiModelProperty("合同编号")
    private String contractId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("车牌号")
    private String licensePlateNumber;

    @ApiModelProperty("产品数量")
    private BigDecimal productNumber;

    @ApiModelProperty("产品单位")
    private String productUnit;

    @ApiModelProperty("吨袋信息")
    private List<SaleOutStorageTonBagDTO> saleOutStorageTonBagDTOS;

    @ApiModelProperty("质检情况")
    private List<SampleQualityTestDTO> sampleQualityTestDTOS;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;

    @ApiModelProperty("销售出库新增出库")
    private FinishedProductFreeOutDetailDTO finishedProductFreeOutDetailDTO;

}
