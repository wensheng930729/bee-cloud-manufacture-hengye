package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import java.io.Serializable;

/**
 * <p>
 * 原料吨耗
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("修改原料吨耗请求参数")
public class ConfigMaterialsConsumptionUpdateRQ implements Serializable {

    private static final long serialVersionUID = 1L;


    @ApiModelProperty("id")
    @NotNull(message = "id不能为空")
    private Integer id;
    /**
     * 产品id
     */
    @ApiModelProperty("产品id")
    @NotNull(message = "产品id不能为空")
    private Integer productId;
    /**
     * 产品名称
     */
    @ApiModelProperty("产品名称")
    @NotEmpty(message = "产品名称不能为空")
    private String productName;







}
