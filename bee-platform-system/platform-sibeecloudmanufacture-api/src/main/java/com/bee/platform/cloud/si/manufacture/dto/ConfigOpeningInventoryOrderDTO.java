package com.bee.platform.cloud.si.manufacture.dto;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.util.Date;
import java.util.List;

/**
 * <p>
 * 期初库存主表
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@NoArgsConstructor
@Data
@Accessors(chain = true)
@ApiModel("期初库存搜索返回信息")
public class ConfigOpeningInventoryOrderDTO implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * id
     */
    @ApiModelProperty("id")
    private Integer id;
    /**
     * 公司id
     */
    @ApiModelProperty("公司id")
    private Integer enterpriseId;
    /**
     * 工厂id
     */
    @ApiModelProperty("工厂id")
    private Integer factoryId;
    /**
     * 期初库存编号
     */
    @ApiModelProperty("期初库存编号")
    private String code;
    /**
     * 期初日期
     */
    @ApiModelProperty("期初日期")
    @JsonFormat(pattern = "yyyy-MM-dd")
    private Date openingInventoryTime;
    /**
     * 备注
     */
    @ApiModelProperty("备注")
    private String remark;

    @ApiModelProperty("详情列表")
    private List<ConfigOpeningInventoryOrderDetailDTO> detailDTOS;


}
