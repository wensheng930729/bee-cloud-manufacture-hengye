import React, { Component } from 'react'
import { Button, Card, Input, Select, message, Table } from 'antd'
import styles from './index.less'
import withRouter from 'umi/withRouter'
import BreadCrumb from '../../../../components/BreadCrumb'
import { updateTestItem, getProductById, getTestAttributeByType } from '../../services'
import * as utils from '@/utils/utils'
import router from 'umi/router'
import constasts from '../const'
const { testUnits, decimalDigits } = constasts
const Option = Select.Option
const mark = () => {
  const result = []
  for (let i = 0; i < 4; i++) {
    let ranNum = Math.ceil(Math.random() * 25) //生成一个0到25的数字
    //大写字母'A'的ASCII是65,A~Z的ASCII码就是65 + 0~25;然后调用String.fromCharCode()传入ASCII值返回相应的字符并push进数组里
    result.push(String.fromCharCode(65 + ranNum))
  }
  const str = result.join('')
  return '${' + str + '}'
}

@withRouter
export default class index extends Component {
  state = {
    dataIn: [
      {
        assayItemIn: '',
        id: utils.guid(),
        markIn: mark(),
        testUnitObj: undefined
      }
    ],
    dataOut: [
      {
        assayItemOut: '',
        id: utils.guid(),
        markOut: mark(),
        testUnitObj: undefined,
        assayFormula: ' ',
        decimalDigit: 2

      }
    ],
    inItem: [],
    outItem: []
  }

  id = this.props.location.query.id || null

  componentDidMount() {
    getProductById(this.id).then(res => {
      if (res.code === 1) {
        const data = res.object
        this.setState({
          dataIn: this.testUnitObjHandle(data.productTestAttributeInDTOS),
          dataOut: this.testUnitObjHandle(data.productTestAttributeOutDTOS)
        })
      }
    })
    getTestAttributeByType({ types: [0] }).then(res => {
      if (res.code === 1) {
        this.setState({
          inItem: res.object
        })
      }
    })
    getTestAttributeByType({ types: [1] }).then(res => {
      if (res.code === 1) {
        this.setState({
          outItem: res.object
        })
      }
    })
  }

  handleSubmit = () => {
    const { dataIn, dataOut } = this.state
    const params = {
      id: this.id,
      productTestAttributeInSaveRQS: this.paramsTestUnitObjHandle(dataIn),
      productTestAttributeOutSaveRQS: this.paramsTestUnitObjHandle(dataOut)
    }
    updateTestItem(params).then(res => {
      if (res.code === 1) {
        message.success(res.message)
        router.goBack()
      } else {
        message.error(res.message)
      }
    })
  }

  //回写参数整理
  testUnitObjHandle(data = []) {
    if (data && data.length) {
      return data.map(item => ({
        ...item,
        testUnitObj: { key: item.testUnit, label: item.unitString }
      }))
    };
    return []
  }

  //提交参数处理
  paramsTestUnitObjHandle(data = []) {
    if (data && data.length) {
      return data.map(item => {
        let newItem = { ...item };
        if (item.testUnitObj) {
          newItem.testUnit = item.testUnitObj.key;
          newItem.unitString = item.testUnitObj.label;
          delete newItem.testUnitObj
        }
        return newItem;
      })
    };
    return []
  }

  //输入项更改
  handleChange = (e, name, dataIndex) => {
    let { dataIn } = this.state
    dataIn[dataIndex][name] = e
    this.setState({
      dataIn
    })
  }

  //结果项更改
  handleChangeOut = (e, name, dataIndex) => {
    const { dataOut } = this.state
    dataOut[dataIndex][name] = e
    this.setState({
      dataOut
    })
  }

  //添加输出项
  handleAdd = () => {
    const { dataIn } = this.state
    // const id = dataIn.length + 1
    const obj = {
      assayItemIn: '',
      id: utils.guid(),
      markIn: mark(),
      testUnitObj: undefined
    }
    dataIn.push(obj)
    this.setState({
      dataIn
    })
  }

  //添加结果项
  handleAddOut = () => {
    const { dataOut } = this.state
    // const id = dataOut.length + 1
    const obj = {
      assayItemOut: '',
      id: utils.guid(),
      markOut: mark(),
      testUnitObj: undefined,
      decimalDigit: 2,
      assayFormula: ''
    }
    dataOut.push(obj)
    this.setState({
      dataOut
    })
  }

  //删除输入项
  handleDelete = index => {
    const { dataIn } = this.state
    dataIn.splice(index, 1)
    this.setState({
      dataIn
    })
  }

  //删除结果项
  handleDeleteOut = index => {
    const { dataOut } = this.state
    dataOut.splice(index, 1)
    this.setState({
      dataOut
    })
  }



  render() {
    const { dataIn, dataOut, inItem, outItem } = this.state
    const self = this
    const columns = [
      {
        title: '化验输入项',
        dataIndex: 'assayItemIn',
        width: '20%',
        render: (h, row, index) => (
          <Select style={{ width: '100%' }} value={h} placeholder="化验输入项" onChange={e => self.handleChange(e, 'assayItemIn', index)}>
            {
              inItem.map(item => <Option value={item.attributeName}>{item.attributeName}</Option>)
            }
          </Select>
        )
      }, {
        title: '输入项字符',
        dataIndex: 'markIn'
      }, {
        title: '单位',
        dataIndex: 'testUnitObj',
        width: '20%',
        render: (h, row, index) => (
          <Select value={h} labelInValue onChange={e => self.handleChange(e, 'testUnitObj', index)} placeholder="单位" style={{ width: '100%' }}>
            {testUnits.map(item => <Option value={item.value} key={item.value}>{item.label}</Option>)}
          </Select>
        )
      }, {
        title: '操作',
        dataIndex: 'action',
        render: (h, row, index) => <span style={{ color: '#1890ff', cursor: 'pointer' }} onClick={() => self.handleDelete(index)}>删除</span>
      }
    ]

    const columnsTwo = [
      {
        title: '化验结果项',
        dataIndex: 'assayItemOut',
        width: '20%',
        render: (h, row, index) => (
          <Select style={{ width: '100%' }} value={h} placeholder="化验结果项" onChange={e => self.handleChangeOut(e, 'assayItemOut', index)}>
            {
              outItem.map(item => <Option value={item.attributeName}>{item.attributeName}</Option>)
            }
          </Select>
        )
      }, {
        title: '结果项字符',
        dataIndex: 'markOut'
      }, {
        title: '化验公式',
        dataIndex: 'assayFormula',
        width: '20%',
        render: (h, row, index) => <Input placeholder="化验公式" value={h} onChange={e => self.handleChangeOut(e.target.value, 'assayFormula', index)} />
      }, {
        title: '单位',
        dataIndex: 'testUnitObj',
        width: '20%',
        render: (h, row, index) => (
          <Select labelInValue value={h} onChange={e => self.handleChangeOut(e, 'testUnitObj', index)} placeholder="单位" style={{ width: '100%' }}>
            {testUnits.map(item => <Option value={item.value} key={item.value}>{item.label}</Option>)}
          </Select>
        )
      }, {
        title: '保留小数位',
        dataIndex: 'decimalDigit',
        width: '20%',
        render: (h, row, index) => (
          <Select value={h} onChange={e => self.handleChangeOut(e, 'decimalDigit', index)} placeholder="小数位" style={{ width: '100%' }}>
            {decimalDigits.map(item => <Option value={item.value} key={item.value}>{item.label}</Option>)}
          </Select>
        )
      }, {
        title: '操作',
        dataIndex: 'action',
        render: (h, row, index) => <span style={{ color: '#1890ff', cursor: 'pointer' }} onClick={() => self.handleDeleteOut(index)}>删除</span>
      }
    ]

    return (
      <div className={styles.wrap}>
        <BreadCrumb extra={<Button type="primary" onClick={() => router.goBack()}>返回</Button>} />
        <div className={styles.container}>
          <Card
            title="化验配置"
            bordered={false}
            extra={
              <div className={styles.btnBox}>
                <Button type="primary" onClick={this.handleSubmit}>
                  {this.id ? '修改' : '保存'}
                </Button>
                <Button onClick={() => router.goBack()}>取消</Button>
              </div>
            }
          >
            <Card title="化验输入项" bordered={false}>
              <Table
                rowKey="id"
                columns={columns}
                dataSource={dataIn}
                pagination={false}
              />
              <div className={styles.btn_add} onClick={this.handleAdd}>
                添加一行
            </div>
            </Card>
            <Card title="化验结果项" bordered={false}>
              <Table
                rowKey="id"
                columns={columnsTwo}
                dataSource={dataOut}
                pagination={false}
              />
              <div className={styles.btn_add} onClick={this.handleAddOut}>
                添加一行
            </div>
            </Card>
          </Card>
        </div>
      </div>
    )
  }
}
