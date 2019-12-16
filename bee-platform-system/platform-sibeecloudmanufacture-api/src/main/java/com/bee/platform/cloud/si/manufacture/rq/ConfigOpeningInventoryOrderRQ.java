package com.bee.platform.cloud.si.manufacture.rq;

import com.fasterxml.jackson.annotation.JsonFormat;
import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotEmpty;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
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
@ApiModel("保存期初库存请求参数")
public class ConfigOpeningInventoryOrderRQ implements Serializable {

    private static final long serialVersionUID = 1L;

    /**
     * 期初库存编号
     */
    @ApiModelProperty("期初库存编号")
    @NotEmpty(message = "期初库存编号不能为空")
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
    @NotNull(message = "详情列表至少一条数据,不能为空")
    @Size(min = 1,message = "详情列表至少一条数据")
    private List<ConfigOpeningInventoryOrderDetailRQ> detailRQS;



}
