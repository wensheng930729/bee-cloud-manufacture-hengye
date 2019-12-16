import React, { Component } from 'react'
import style from './index.less'
import propTypes from "prop-types"
import { Button, Icon, Row, Col } from "antd"

class ChangeroleList extends Component {
  constructor() {
    super()
  }
  render() {

    const { permissionId, permissionList } = this.props;
    const colProps = { xxl: 6, xl: 6, lg: 8, md: 12, sm: 12, xs: 12 };
    return (
      <div className={style.wrapper}>
        <div className={style.content} style={{ position: "relative" }}>
          <div className={style.header}>
            请选择角色
        </div>
          <Row className={style.buttons} gutter={15} >
            {permissionList && permissionList.length ?
              permissionList.map(item => {
                return (<Col {...colProps} key={item.permissionId}>
                  <Button type={"primary"} ghost style={{ height: 40, lineHeight: 40 }} onClick={() => this.props.pickRole(item)}
                    style={{ borderStyle: item.permissionId === Number(permissionId) ? 'solid' : 'dashed' }}>
                    {item.permissionId === Number(permissionId) && <Icon type="check-circle" theme="twoTone" twoToneColor="#52c41a" />}
                    <span>
                      {item.permissionName}
                    </span>
                  </Button>
                </Col>)
              }) : null
            }
          </Row>
          <div className={style.into}><Button type="primary" size="small" onClick={this.props.go.bind(this)}>{!permissionList || !permissionList.length ? '立即认证' : '立即进入'}</Button></div>
        </div>
      </div>
    )
  }
}

ChangeroleList.propTypes = {
  go: propTypes.func,
}

//参数字段
//go 点击进入时回调函数 参数为选择角色的id
//user 用户角色参数
//roleList 用户拥有的权限角色
//name 角色名称
//id 角色id
//roleId 用户当前角色id
ChangeroleList.defaultProps = {
  go: (state) => {
  },
  permissionList: [],
  permissionId: 0,
  pickRole: () => { false }
}

export default ChangeroleList