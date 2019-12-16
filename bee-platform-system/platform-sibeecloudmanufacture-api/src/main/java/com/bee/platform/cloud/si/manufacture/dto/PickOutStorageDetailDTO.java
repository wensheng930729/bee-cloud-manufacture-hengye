package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * <p>
 * 新增出库详情
 * </p>
 *
 * @author MP123
 * @since 2019-09-27
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("新增出库详情")
public class PickOutStorageDetailDTO implements Serializable {

    private static final long serialVersionUID = 1L;


    @ApiModelProperty("id")
    private Long id;

    @ApiModelProperty("企业id")
    private Integer enterpriseId;

    @ApiModelProperty("工厂id")
    private Integer factoryId;

    @ApiModelProperty("仓库id")
    private Integer storageId;

    @ApiModelProperty("仓库名称")
    private String storageName;

    @ApiModelProperty("产品id")
    private Integer productId;

    @ApiModelProperty("产品名称")
    private String productName;

    @ApiModelProperty("出库数量")
    private BigDecimal productNumber;

    @ApiModelProperty("产品规格名称")
    private String productSpecName;

    @ApiModelProperty("领用人")
    private String receiver;

    @ApiModelProperty("出库时间")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private LocalDateTime receiveTime;




}
