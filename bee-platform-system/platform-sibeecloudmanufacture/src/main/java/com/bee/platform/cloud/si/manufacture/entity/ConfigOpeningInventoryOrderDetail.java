package com.bee.platform.cloud.si.manufacture.entity;

import cn.afterturn.easypoi.excel.annotation.Excel;
import com.baomidou.mybatisplus.activerecord.Model;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.fasterxml.jackson.annotation.JsonFormat;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.NoArgsConstructor;
import lombok.experimental.Accessors;

import java.io.Serializable;
import java.math.BigDecimal;
import java.util.Date;

/**
 * <p>
 * 期初库存明细表
 * </p>
 *
 * @author chenxm66777123
 * @since 2019-09-23
 */
@Data
@NoArgsConstructor
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class ConfigOpeningInventoryOrderDetail extends Model<ConfigOpeningInventoryOrderDetail> {

    private static final long serialVersionUID = 1L;


    /**
     * 导出类型 1 是文本 2 是图片,3 是函数,10 是数字 默认是文本
     */

    /**
     * id
     */
    @TableId(value = "id", type = IdType.AUTO)
    @Excel(name = "id", orderNum = "1", type = 10, width = 20)
    private Integer id;
    /**
     * 公司id
     */
    @Excel(name = "公司id", orderNum = "2", type = 10, width = 20)
    private Integer enterpriseId;
    /**
     * 工厂id
     */
    @Excel(name = "工厂id", orderNum = "3", type = 10, width = 20)
    private Integer factoryId;
    /**
     * 期初库存表id
     */
    @Excel(name = "期初库存表id", orderNum = "4", type = 10, width = 20)
    private Integer openingInventoryOrderId;
    /**
     * 产品_id
     */
    @Excel(name = "产品_id", orderNum = "5", type = 10, width = 20)
    private Integer productId;
    /**
     * 产品名称
     */
    @Excel(name = "产品名称" ,orderNum = "6",width = 20)
    private String productName;


    /**
     * 产品规格id
     */
    @Excel(name = "产品规格id", orderNum = "7", type = 10, width = 20)
    private Integer productSpecId;
    /**
     * 产品规格名称
     */
    @Excel(name = "产品规格名称" ,orderNum = "8",width = 20)
    private String productSpecName;
    /**
     * 仓库id
     */
    @Excel(name = "仓库id", orderNum = "9", type = 10, width = 20)
    private Integer repositoryId;
    /**
     * 仓库名称
     */
    @Excel(name = "仓库名称" ,orderNum = "10",width = 20)
    private String repositoryName;
    /**
     * 计量单位
     */
    @Excel(name = "计量单位" ,orderNum = "11",width = 20)
    private String unit;
    /**
     * 化验结果
     */
    @Excel(name = "化验结果（合格/不合格）" ,orderNum = "12",width = 20)
    private String testResult;
    /**
     * 期初数量
     */
    @Excel(name = "期初数量", orderNum = "13", type = 10, width = 20)
    private BigDecimal quantity;
    /**
     * 期初单价
     */
    @Excel(name = "期初单价", orderNum = "14", type = 10, width = 20)
    private BigDecimal price;
    /**
     * 期初金额
     */
    @Excel(name = "期初金额", orderNum = "15", type = 10, width = 20)
    private BigDecimal money;
    /**
     * 逻辑删除字段，1删除，0未删除
     */
    @Excel(name = "逻辑删除字段，1删除，0未删除", orderNum = "16", type = 10, width = 20)
    private Integer deleted;
    /**
     * 创建人id
     */
    @Excel(name = "创建人id", orderNum = "17", type = 10, width = 20)
    private Integer createId;
    /**
     * 创建人名称
     */
    @Excel(name = "创建人名称" ,orderNum = "18",width = 20)
    private String creator;
    /**
     * 创建时间
     */
    @Excel(name = "创建时间",orderNum = "19",width = 20, format = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date createTime;
    /**
     * 修改人id
     */
    @Excel(name = "修改人id", orderNum = "20", type = 10, width = 20)
    private Integer modifyId;
    /**
     * 修改人名称
     */
    @Excel(name = "修改人名称" ,orderNum = "21",width = 20)
    private String modifier;
    /**
     * 修改时间
     */
    @Excel(name = "修改时间",orderNum = "22",width = 20, format = "yyyy-MM-dd HH:mm:ss")
    @JsonFormat(pattern = "yyyy-MM-dd HH:mm:ss")
    private Date modifyTime;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
