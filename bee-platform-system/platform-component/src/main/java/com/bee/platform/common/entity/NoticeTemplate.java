package com.bee.platform.common.entity;

import java.io.Serializable;

import com.baomidou.mybatisplus.annotations.TableName;
import com.baomidou.mybatisplus.enums.IdType;
import java.util.Date;
import com.baomidou.mybatisplus.annotations.TableId;
import com.baomidou.mybatisplus.activerecord.Model;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import lombok.ToString;
import lombok.experimental.Accessors;

/**
 * 后台管理系统通知模板
 *
 * @author junyang.li
 * @since 2019-05-06
 */

@Getter
@Setter
@ToString
@NoArgsConstructor
@Accessors(chain = true)
@TableName("m_notice_template")
public class NoticeTemplate extends Model<NoticeTemplate> {

    private static final long serialVersionUID = 1L;

    /**
     * 模板id
     */
    @TableId(value = "template_id", type = IdType.AUTO)
    private Integer templateId;
    /**
     * 模板名称
     */
    private String templateName;
    /**
     * 模板类型
     */
    private Integer templateType;
    /**
     * 通知标题
     */
    private String noticeTitle;
    /**
     * 通知对象
     */
    private String notifyObject;
    /**
     * 通知对象类型
     */
    private Integer notifyObjectType;
    /**
     * 模板内容
     */
    private String templateContent;
    /**
     * 通知方式
     */
    private Integer notifyMode;
    /**
     * 备注
     */
    private String notes;
    /**
     * 最后编辑时间
     */
    private Date updateTime;
    /**
     * 是否有效
     */
    private Integer status;

    @Override
    protected Serializable pkVal() {
        return this.templateId;
    }

}
