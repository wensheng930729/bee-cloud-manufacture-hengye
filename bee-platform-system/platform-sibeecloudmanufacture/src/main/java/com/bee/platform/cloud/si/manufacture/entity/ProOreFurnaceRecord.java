package com.bee.platform.cloud.si.manufacture.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.enums.IdType;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.enums.IdType;
import com.baomidou.mybatisplus.activerecord.Model;
import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.Version;

import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.experimental.Accessors;

/**
 * <p>
 * 矿热炉记录
 * </p>
 *
 * @author huangxin123
 * @since 2019-09-26
 */
@Data
@EqualsAndHashCode(callSuper = true)
@Accessors(chain = true)
public class ProOreFurnaceRecord extends Model<ProOreFurnaceRecord> {

    private static final long serialVersionUID = 1L;

    @TableId(value = "id", type = IdType.AUTO)
    private Long id;
    /**
     * 炉号id
     */
    private Integer furnaceId;
    /**
     * 炉号名称
     */
    private String furnaceName;
    /**
     * 班次:1一班，2二班，3三班
     */
    private Integer shift;
    /**
     * 创建时间
     */
    private Date createTime;
    /**
     * 更新时间
     */
    private Date modifyTime;
    /**
     * 创建人id
     */
    private Integer createId;
    /**
     * 修改人id
     */
    private Integer modifyId;
    /**
     * 工厂id
     */
    private Integer factoryId;
    /**
     * 企业id
     */
    private Integer companyId;
    /**
     * 当前是否有人值班：1是，0否
     */
    private Integer onduty;
    /**
     * 数据状态:0删除1正常
     */
    private Integer status;
    /**
     * 开班时间
     */
    private Date openTime;


    @Override
    protected Serializable pkVal() {
        return this.id;
    }

}
