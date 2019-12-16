package com.bee.platform.cloud.si.manufacture.rq;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.*;
import java.io.Serializable;
import java.util.List;

/**
 * <p>
 * 产品档案
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("产品修改请求参数")
public class ConfigProductUpdateRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    @ApiModelProperty("id")
    @NotNull(message = "id不能为空")
    private Integer id;

    /**
     * 产品名称
     */
    @ApiModelProperty("产品名称")
    @NotEmpty(message = "产品名称不能为空")
    private String name;
    /**
     * 产品logo
     */
    @ApiModelProperty("产品logo")
    private String logo;
    /**
     * 单位code
     */
    @ApiModelProperty("单位code")
    @NotEmpty(message = "单位code不能为空")
    private String unitCode;
    /**
     * 单位value
     */
    @ApiModelProperty("单位value")
    @NotEmpty(message = "单位value不能为空")
    private String unitValue;
    /**
     * 产品类别
     */
    @ApiModelProperty("产品类别")
    @NotNull(message = "产品类别id不能为空")
    private Integer categoryId;
    /**
     * 产品类别名称
     */
    @ApiModelProperty("产品类别名称")
    @NotEmpty(message = "产品类别名称不能为空")
    private String categoryName;
    /**
     * 状态:1-启用,0-禁用
     */
    @ApiModelProperty("状态:1-启用,0-禁用")
    @NotNull(message = "状态不能为空")
    private Integer status;

    /**
     * 是否是标准品（0 否 1 是）
     */
    @ApiModelProperty("是否是标准品（0 否 1 是）")
    @NotNull(message = "是否是标准品不能为空")
    @Min(value = 0,message = "是否是标准品只能为0或1")
    @Max(value = 1,message = "是否是标准品只能为0或1")
    private Integer standard;

    @ApiModelProperty("产品结算属性列表")
//    @NotNull(message = "产品结算属性列表至少一条数据,不能为空")
//    @Size(min = 1,message = "产品结算属性至少一条数据")
    private List<ConfigProductSettlementAttributeSaveRQ> productSettlementAttributeSaveRQS;




}
