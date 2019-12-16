import React, { Component } from 'react'
import router from 'umi/router'


import { connect } from 'dva'

@connect(({ global }) => ({
  global
  // logining: loading.effects['global/login']
}))
export default class Index extends Component {
  componentWillMount() {
    if (!this.props.global.login)
    router.push('/login')
  }

  render() {
    return null
  }
}
